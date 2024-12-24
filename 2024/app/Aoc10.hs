module Aoc10 where

import AocBase (Main)
import qualified Grids
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.List (nub)

type Cell = Int
type Grid = Grids.Grid Cell

solve :: Main
solve input = do
  let grid = parseInput input
  putStrLn "Part 1"
  res1 <- part1 grid
  putStrLn $ "Result: " ++ show res1
  putStrLn "Part 2"
  print $ part2 grid
  putStrLn "Done"

part1 :: Grid -> IO Int
part1 grid = do
  let paths = map (pathsFromCoord grid) (startingPoints grid)
  print paths
  let scores = map scoreForPaths paths
  print scores
  pure $ sum scores

part2 :: Grid -> Int
part2 = undefined

scoreForPaths :: [[Grids.Coord]] -> Int
scoreForPaths paths = length $ nub startEndPair
  where
    fullLengthPaths = filter ((==) 10 . length) paths
    startEndPair = map (\p -> (head p, last p)) fullLengthPaths

pathsFromCoord :: Grid -> Grids.Coord -> [[Grids.Coord]]
pathsFromCoord grid coord 
  | null reachable = [[coord]]
  | otherwise = map (coord:) (concatMap (pathsFromCoord grid) reachable)
  where
    reachable = nextCoords grid coord

nextCoords :: Grid -> Grids.Coord -> [Grids.Coord]
nextCoords grid coord = map fst (filter ((==) successor . snd) cells)
  where
    adjacents = adjacentCoords grid coord
    successor = succ (grid Map.! coord)
    cells = map (\c -> (c, grid Map.! c)) adjacents

adjacentCoords :: Grid -> Grids.Coord -> [Grids.Coord]
adjacentCoords grid coord = filter (`Map.member` grid) $ map (Grids.moveCoord coord) [Grids.North, Grids.East, Grids.South, Grids.West]

startingPoints :: Grid -> [Grids.Coord]
startingPoints = Map.keys . Map.filter (== 0)

parseInput :: [String] -> Grid
parseInput = Grids.parseInput cellFromChar

cellFromChar :: Char -> Cell
cellFromChar = digitToInt
