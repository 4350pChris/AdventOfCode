module Aoc1 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)

main :: [String] -> IO ()
main input = do
  putStrLn "Part 1"
  part1 input
  putStrLn "Part 2"
  part2 input

-- each row contains two 5-digit numbers separated by 4 spaces
parseInput :: [String] -> ([Int], [Int])
parseInput rows = do
  let numbers = map words rows
  -- take first and second element of each row explicitly
  let firs = map (read . head) numbers
      secs = map (read . (!! 1)) numbers
  (firs, secs)

part1 :: [String] -> IO ()
part1 input = do
  -- we want to sort both lists then sum the differences
  let sorted = bimap sort sort (parseInput input)
      result = sum . map abs $ uncurry (zipWith (-)) sorted
  print result

part2 :: [String] -> IO ()
part2 rows = do
  -- for each element in the first list, find out how often it occurs in the second list
  let input = parseInput rows
      intAndOccurenceTuples = map (\x -> (x, length . filter (== x) $ snd input)) (fst input)
      -- multiply each element with its occurence, then sum the results
      result = sum $ map (uncurry (*)) intAndOccurenceTuples
  print result
