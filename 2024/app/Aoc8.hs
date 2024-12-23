{-# LANGUAGE TupleSections #-}

module Aoc8 where

import AocBase (Main)
import qualified Data.Bifunctor
import Data.List (nub)
import qualified Data.Map as Map

type Coord = (Int, Int)

type Grid = Map.Map Coord Char

type Vector = (Int, Int)

solve :: Main
solve input = do
  let parsed = parseInput input
  putStrLn "Part 1"
  print $ part1 parsed
  putStrLn "Part 2"
  print $ part2 parsed
  putStrLn "Done"

part1 :: Grid -> Int
part1 grid = length . nub . concatMap (findAntinodes grid) $ pairsForGrid grid

part2 :: Grid -> Int
part2 = undefined

pairsForGrid :: Grid -> [(Coord, Coord)]
pairsForGrid grid = makePairs [k | (k, v) <- asList, char <- Map.elems grid, v == char]
  where
    asList = Map.toList grid
    makePairs [] = []
    makePairs (x : xs) = map (x,) xs ++ makePairs xs

-- for every two points there exist two antinodes
-- an antinode is a point where the second point is double the distance away from the antinode than the first point
findAntinodes :: Grid -> (Coord, Coord) -> [Coord]
findAntinodes grid (a, b) = filter (`Map.member` grid) [a1, a2]
  where
    vec = Data.Bifunctor.bimap (fst b -) (snd b -) a
    walk (x, y) (dx, dy) = (x + dx, y + dy)
    (a1, a2) = (walk a (Data.Bifunctor.bimap negate negate vec), walk b vec)

parseInput :: [String] -> Grid
parseInput = Map.fromList . concatMap parseLine . zip [0 ..]
  where
    parseLine :: (Int, String) -> [(Coord, Char)]
    parseLine (y, line) = [((x, y), c) | (x, c) <- zip [0 ..] line]
