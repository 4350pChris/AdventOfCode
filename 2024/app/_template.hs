module Aoc0 where

import AocBase (Main)

solve :: Main
solve input = do
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 input
  putStrLn "Done"

part1 :: a -> b
part1 = undefined

part2 :: a -> b
part2 = undefined

parseInput :: [String] -> a
