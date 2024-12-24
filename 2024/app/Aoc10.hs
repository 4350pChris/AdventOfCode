module Aoc10 where

import AocBase (Main)
import qualified Grids

type Grid = Grids.Grid Int

solve :: Main
solve input = do
  let grid = parseInput input
  putStrLn "Part 1"
  print $ part1 grid
  putStrLn "Part 2"
  print $ part2 grid
  putStrLn "Done"

part1 :: Grid -> Int
part1 = undefined

part2 :: Grid -> Int
part2 = undefined

parseInput :: [String] -> Grid
parseInput = Grids.parseInput cellFromChar

cellFromChar :: Char -> Int
cellFromChar = read . show
