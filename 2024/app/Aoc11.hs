module Aoc11 where

import AocBase (Main)
import qualified Data.Map as Map

type MemoMap = Map.Map (Int, Int) Int

solve :: Main
solve input = do
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 25 parsed
  putStrLn "Part 2"
  print $ part2 75 parsed
  putStrLn "Done"

part1 :: Int -> [Int] -> Int
part1 steps nums = sum $ map (\n -> fst $ expand n steps Map.empty) nums

part2 :: Int -> [Int] -> Int
part2 = part1

expand :: Int -> Int -> MemoMap -> (Int, MemoMap)
expand n steps memo
  | steps == 0 = (1, memo)
  | otherwise = case Map.lookup (n, steps) memo of
      Just result -> (result, memo)
      Nothing -> 
        let (result, finalMemo) = case () of
              _ | n == 0 -> 
                    let (r, m) = expand 1 (steps - 1) memo
                    in (r, Map.insert (n, steps) r m)
              _ | even (length (show n)) -> 
                    let (left, right) = splitNumber n
                        (r1, m1) = expand left (steps - 1) memo
                        (r2, m2) = expand right (steps - 1) m1
                        total = r1 + r2
                    in (total, Map.insert (n, steps) total m2)
              _ -> let (r, m) = expand (n * 2024) (steps - 1) memo
                   in (r, Map.insert (n, steps) r m)
        in (result, finalMemo)

runTick :: [Int] -> [Int]
runTick = concatMap change

change :: Int -> [Int]
change n
  | n == 0 = [1]
  | even (length (show n)) = let (a, b) = splitNumber n in [a, b]
  | otherwise = [n * 2024]

splitNumber :: Int -> (Int, Int)
splitNumber n = (read a, read b)
  where
    (a, b) = splitAt (length (show n) `div` 2) (show n)

parseInput :: [String] -> [Int]
parseInput = map read . words . head
