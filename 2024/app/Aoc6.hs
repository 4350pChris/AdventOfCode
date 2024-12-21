module Aoc6 where

import AocBase (Main)
import qualified Data.Map as Map
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

main :: Main
main input = do
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
part2 = length . allLoops . fst . last . runUntilOutside

-- for part 2 we need to find loops we can create by inserting an obstacle
-- we can do this by finding 3 obstacles that are in a certain position

allLoops :: Grid -> [Maybe (Coord, Coord, Coord, Coord)]
allLoops grid = concatMap (filter isJust . findObstacleLoop grid) (everyObstacle grid)

everyObstacle :: Grid -> [Coord]
everyObstacle = Map.keys . Map.filter (== Obstacle)

findObstacleLoop :: Grid -> Coord -> [Maybe (Coord, Coord, Coord, Coord)]
findObstacleLoop grid coord = filter isJust possible
  where
    possible = map (\dir -> findPossibleLoops grid coord dir []) [North, East, South, West]

-- from a list of 3 coords, find the missing corner to make it a rectangle
findMissingCorner :: (Coord, Coord, Coord) -> Coord
findMissingCorner ((x1, y1), (x2, y2), (x3, y3)) = (x, y)
  where
    x
      | x1 == x2 = x3
      | x1 == x3 = x2
      | otherwise = x1
    y
      | y1 == y2 = y3
      | y1 == y3 = y2
      | otherwise = y1

findPossibleLoops :: Grid -> Coord -> Direction -> [Coord] -> Maybe (Coord, Coord, Coord, Coord)
findPossibleLoops _ _ _ [a, b, c] = Just (a, b, c, findMissingCorner (a, b, c))
findPossibleLoops grid coord dir carry = do
  let newDir = turnRight dir
      -- move in the opposite direction of our original direction to get the coord where the guard would turn
      nextCoord = moveCoord coord (turnRight newDir)
  nextObstacle <- findObstacle grid nextCoord newDir
  findPossibleLoops grid nextObstacle newDir (nextCoord : carry)

-- move in a direction until we find an obstacle
findObstacle :: Grid -> Coord -> Direction -> Maybe Coord
findObstacle grid (x, y) dir = do
  cell <- Map.lookup (x, y) grid
  case cell of
    Obstacle -> Just (x, y)
    Unvisited -> Nothing
    Visited -> findObstacle grid (x + dx, y + dy) dir
  where
    (dx, dy) = movementDelta dir

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
