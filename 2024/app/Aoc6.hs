module Aoc6 where

import AocBase (Main)
import qualified Data.Map as Map
import Data.Bifunctor (first)
import Data.Maybe (isJust)

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

movementDelta :: Direction -> Coord
movementDelta North = (0, -1)
movementDelta East = (1, 0)
movementDelta South = (0, 1)
movementDelta West = (-1, 0)

moveCoord :: Coord -> Direction -> Coord
moveCoord (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = movementDelta dir

move :: Guard -> Guard
move (Guard coord dir) = Guard (moveCoord coord dir) dir

solve :: Main
solve input = do
  let grid = parseInput input
      guard = Guard (startingCoord grid) North
      initialState = (grid, guard)
  putStrLn "Part 1"
  print $ part1 initialState

  putStrLn "Part 2"
  print $ part2 initialState
  putStrLn "Done"

-- run until guard leaves the grid
part1 :: CurrentState -> Int
part1 = length . everyVisitedCell . fst . last . runUntilOutside

part2 :: CurrentState -> Int
part2 = length . filter (isJust . floydTortoiseAndHare) . withObstacles

-- along each visited cell, we add an obstacle
withObstacles :: CurrentState -> [CurrentState]
withObstacles initial = map (\cell -> first (Map.insert cell Obstacle) initial) . everyVisitedCell . fst . last $ runUntilOutside initial

floydTortoiseAndHare :: CurrentState -> Maybe Coord
floydTortoiseAndHare state = do
  -- first we let the hare run twice as fast as the tortoise
  -- we use the original guard as the tortoise
  -- and a new guard that moves twice as fast as the original guard as the hare
  -- we run the hare twice as fast as the tortoise untile they meet
  hare <- runUntilMeet (runTick state) (advanceHare state) advanceHare
  -- now find the first repetition by resetting the tortoise and letting them run at the same speed
  findIntersection state hare
  where advanceHare = runTick . runTick

-- do runUntilMeet with the tortoise and hare running at the same speed
findIntersection :: CurrentState -> CurrentState -> Maybe Coord
findIntersection tortoise hare = runUntilMeet tortoise hare runTick >>= \(_, guard) -> Just (position guard)

runUntilMeet :: CurrentState -> CurrentState -> (CurrentState -> CurrentState) -> Maybe CurrentState
runUntilMeet tortoise hare advanceHare
  -- if one runs out of bounds, we can stop
  | not (withinGrid tortoise) || not (withinGrid hare) = Nothing
  | position (snd tortoise) == position (snd hare) && facing (snd tortoise) == facing (snd hare) = Just tortoise
  | otherwise = runUntilMeet (runTick tortoise) (advanceHare hare) advanceHare

runUntilOutside :: CurrentState -> [CurrentState]
runUntilOutside = takeWhile withinGrid . iterate runTick

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
  putStrLn "-------"
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
