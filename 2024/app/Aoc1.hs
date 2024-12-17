module Aoc1 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)
import Input (parseInputFromFile)

main :: IO ()
main = do
  putStrLn "Part 1"
  part1
  putStrLn "Part 2"
  part2

-- each row contains two 5-digit numbers separated by 4 spaces
parseInput :: String -> IO ([Int], [Int])
parseInput filename = do
  rows <- parseInputFromFile filename
  let numbers = map words rows
  -- take first and second element of each row explicitly
  let firs = map (read . head) numbers
  let secs = map (read . (!! 1)) numbers
  let parsed = (firs, secs)
  return parsed

part1 :: IO ()
part1 = do
  input <- parseInput "inputs/1.txt"
  -- input is a list of tuples
  -- we want to sort both lists then sum the differences
  let sorted = bimap sort sort input
      result = sum . map abs $ uncurry (zipWith (-)) sorted
  print result

part2 :: IO ()
part2 = do
  input <- parseInput "inputs/1.txt"
  -- for each element in the first list, find out how often it occurs in the second list
  let intAndOccurenceTuples = map (\x -> (x, length . filter (== x) $ snd input)) (fst input)
      -- multiply each element with its occurence, then sum the results
      result = sum $ map (uncurry (*)) intAndOccurenceTuples
  print result
