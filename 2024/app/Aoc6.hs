module Aoc6 where

import AocBase (Main)
import qualified Data.Map as Map

data Direction = North | East | South | West
  deriving (Eq, Show)

data Cell = Unvisited | Visited | Obstacle
  deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Map.Map Coord Cell

data Guard = Guard
  { position :: Coord,
    facing :: Direction
  }

type CurrentState = (Grid, Guard)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnGuardRight :: Guard -> Guard
turnGuardRight guard = guard {facing = turnRight (facing guard)}

move :: Guard -> Guard
move (Guard (x, y) dir) = Guard (x + dx, y + dy) dir
  where
    (dx, dy) = case dir of
      North -> (0, -1)
      East -> (1, 0)
      South -> (0, 1)
      West -> (-1, 0)

main :: Main
main input = do
  let grid = parseInput input
      guard = Guard (startingCoord grid) North
      initialState = (grid, guard)
  putStrLn "Part 1"
  -- print $ part1 initialState
  let states = runUntilOutside initialState
  -- printGrid for all states where we could not move
  mapM_ (printGrid . fst) (filter (not . canMove) states)

  print $ length . everyVisitedCell . fst . last $ states

  putStrLn "Part 2"
  -- print $ part2 input
  putStrLn "Done"

-- run until guard leaves the grid
part1 :: CurrentState -> Int
part1 grid = length . everyVisitedCell . fst . last $ runUntilOutside grid

part2 :: CurrentState -> IO Int
part2 = undefined

runUntilOutside :: CurrentState -> [CurrentState]
runUntilOutside state = takeWhile withinGrid $ iterate runTick state

runTick :: CurrentState -> CurrentState
runTick (grid, guard) = (newGrid, newGuard)
  where
    newGuard = if canMove (grid, move guard) then move guard else turnGuardRight guard
    newGrid = if withinGrid (grid, newGuard) then Map.insert (position newGuard) Visited grid else grid

printGrid :: Grid -> IO ()
printGrid grid = do
  let ks = Map.keys grid
      height = maximum $ map snd ks
  mapM_ printLine [0 .. height]
  where
    printLine y = do
      mapM_ (printCell y) [0 .. maximum (map fst (Map.keys grid))]
      putStrLn ""
    printCell y x = case (Map.!) grid (x, y) of
      Unvisited -> putStr "."
      Visited -> putStr "X"
      Obstacle -> putStr "#"

withinGrid :: CurrentState -> Bool
withinGrid (grid, guard) = Map.member (position guard) grid

canMove :: CurrentState -> Bool
canMove state = case currentCell state of
  Nothing -> True
  Just cell -> case cell of
    Obstacle -> False
    _ -> True

currentCell :: CurrentState -> Maybe Cell
currentCell (grid, guard) = Map.lookup (position guard) grid

everyVisitedCell :: Grid -> [Coord]
everyVisitedCell = Map.keys . Map.filter (== Visited)

startingCoord :: Grid -> Coord
startingCoord = head . everyVisitedCell

parseInput :: [String] -> Grid
parseInput input = Map.fromList $ do
  (y, line) <- zip [0 ..] input
  (x, char) <- zip [0 ..] line
  pure ((x, y), cellFromChar char)

cellFromChar :: Char -> Cell
cellFromChar '.' = Unvisited
cellFromChar '#' = Obstacle
cellFromChar _ = Visited
