module Aoc2 where

import Input (parseInputForDay)

main :: IO ()
main = do
  putStrLn "Part 1"
  part1
  putStrLn "Part 2"
  part2

parseInput :: IO [[Int]]
parseInput = do
  contents <- parseInputForDay "2"
  -- contents is a list of strings, where each string is a row consisting of multiple numbers separated by spaces
  -- we want to split each row into a list of numbers
  return $ map (map read . words) contents

allEitherIncreasingOrDecreasing :: [Int] -> Bool
allEitherIncreasingOrDecreasing deltas =
  let increasing = all (>= 0) deltas
      decreasing = all (<= 0) deltas
   in increasing || decreasing

between1And3 :: Int -> Bool
between1And3 x = x <= 3 && x > 0

-- a row is safe when
-- 1. the numbers are all either decreasing or increasing
-- 2. the numbers are all no more than 3 and more than 1 apart
isSafe :: [Int] -> Bool
isSafe row =
  let deltas = zipWith (-) (tail row) row
      orderOk = allEitherIncreasingOrDecreasing deltas
      differenceSmallEnough = all (between1And3 . abs) deltas
   in orderOk && differenceSmallEnough

rowPermutations :: [Int] -> [[Int]]
rowPermutations row = do
  -- for each index in the row, remove the element at that index
  -- this will create a list of lists, where each list is a row with one element removed
  index <- [0 .. length row - 1]
  return $ take index row ++ drop (index + 1) row

part1 :: IO ()
part1 = do
  input <- parseInput
  -- we want to filter out the rows that are safe
  let safeRows = filter isSafe input
  print $ length safeRows

part2 :: IO ()
part2 = do
  -- same as part1, except if we can remove a single number from a row and it becomes safe, we want to count that row
  input <- parseInput
  -- for each row, check itself and all possible rows that can be created by removing a single number
  let safeRows = filter (any isSafe . rowPermutations) input
  print $ length safeRows
