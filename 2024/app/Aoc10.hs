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
  res2 <- part2 grid
  putStrLn $ "Result: " ++ show res2
  putStrLn "Done"

part1 :: Grid -> IO Int
part1 grid = do
  let paths = map (pathsFromCoord grid) (startingPoints grid)
  let pairs = map startEndPairs paths
  let uniquePairs = nub $ concat pairs
  pure $ length uniquePairs

part2 :: Grid -> IO Int
part2 grid = do
  let paths = map (pathsFromCoord grid) (startingPoints grid)
  let pairs = map startEndPairs paths
  pure $ length $ concat pairs

startEndPairs :: [[Grids.Coord]] -> [(Grids.Coord, Grids.Coord)]
startEndPairs = map (\p -> (head p, last p)) . filter ((==) 10 . length)

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
