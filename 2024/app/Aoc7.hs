module Aoc7 where

import AocBase (Main)
import qualified Data.Bifunctor
import Data.List.Split (splitOn)

type Equation = (Int, [Int])

type Operation = (Int -> Int -> Int)

solve :: Main
solve input = do
  let equations = parseInput input
  print equations
  putStrLn "Part 1"
  -- all that are not empty are valid solutions
  print $ part1 equations
  putStrLn "Part 2"
  -- print $ part2 input
  putStrLn "Done"

part1 :: [Equation] -> Int
part1 equations = sum solutions
  where
    evaled = map (Data.Bifunctor.second tryAllOperations) equations
    -- filter out all that have no solution by checking if the first element of the tuple is equal to any of the rhs
    solutions = [lhs | (lhs, ops) <- evaled, lhs `elem` map fst ops]

part2 :: a -> b
part2 = undefined

-- go over all possible combinations of operations for this equation and return all combinations that actually work
tryAllOperations :: [Int] -> [(Int, [Operation])]
tryAllOperations ints = [(evalEquation ints op, op) | op <- ops]
  where
    ops = allOperations (length ints)

evalEquation :: [Int] -> [Operation] -> Int
evalEquation (x : y : xs) (op : ops) = evalEquation (op x y : xs) ops
evalEquation [x] _ = x
evalEquation _ _ = error "Invalid input"

-- all possible combinations of operations for a given length
-- e.g. allOperations 2 == [[(+), (-)], [(+), (*)], [(+), (/)], [(-), (+)], ...]
allOperations :: Int -> [[Operation]]
allOperations n = mapM (const [(+), (*), combineNumbers]) [1 .. n - 1]
  where
    combineNumbers :: Int -> Int -> Int
    combineNumbers x y = read $ show x ++ show y

parseInput :: [String] -> [Equation]
parseInput = map parseEquation

-- parseEquation "190: 10 19" == (190, [10, 19])
parseEquation :: String -> Equation
parseEquation s = (lhs, rhs)
  where
    sp = splitOn ": " s
    lhs = read $ head sp
    rhs = map read $ words $ map (\c -> if c == ',' then ' ' else c) $ last sp
