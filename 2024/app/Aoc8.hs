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
  -- Aoc6.printGrid parsed printCell
  putStrLn "Part 1"
  let res1 = part1 parsed
  print $ length res1
  putStrLn "Part 2"
  let res2 = part2 parsed
  print $ length res2
  putStrLn "Done"

part1 :: Grid -> [Coord]
part1 grid = nub . concatMap (findAntinodes grid) $ pairsForGrid grid

part2 :: Grid -> [Coord]
part2 grid = nub . concatMap (findPart2Antinodes grid) $ pairsForGrid grid

printCell :: Grid -> Int -> Int -> IO ()
printCell grid x y = putStr [Map.findWithDefault '.' (x, y) grid]

pairsForGrid :: Grid -> [(Coord, Coord)]
pairsForGrid grid = concatMap (makePairs . snd) coordsByChar
  where
    asList = Map.toList grid
    coordsByChar = Map.toList $ Map.fromListWith (++) [(v, [k]) | (k, v) <- asList, v /= '.']
    makePairs [] = []
    makePairs (x : xs) = concatMap (\i -> [(i,x), (x,i)]) xs ++ makePairs xs

-- for every two points there exist two antinodes
-- an antinode is a point where the second point is double the distance away from the antinode than the first point
findPart1Antinodes :: Grid -> (Coord, Coord) -> [Coord]
findPart1Antinodes = findAntinodes

-- for part 2 we recursively find the antinodes of the antinodes
findPart2Antinodes :: Grid -> (Coord, Coord) -> [Coord]
findPart2Antinodes grid (a, b) = b : recurseNodes (a, b)
  where
    -- while we can generate antinodes, keep recursing using he new antinode as b
    recurseNodes :: (Coord, Coord) -> [Coord]
    recurseNodes (x, y) = case findAntinodes grid (x, y) of
      [] -> []
      (antinode : _) -> antinode : recurseNodes (y, antinode)

findAntinodes :: Grid -> (Coord, Coord) -> [Coord]
findAntinodes grid (a, b) = filter (`Map.member` grid) [node]
  where
    (dx, dy) = Data.Bifunctor.bimap (fst b -) (snd b -) a
    node = (fst b + dx, snd b + dy)

parseInput :: [String] -> Grid
parseInput input = Map.fromList $ do
  (y, line) <- zip [0 ..] input
  (x, char) <- zip [0 ..] line
  pure ((y, x), char)
