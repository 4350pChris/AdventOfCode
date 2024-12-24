module Grids where

import qualified Data.Map as Map

data Direction = North | East | South | West
  deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Map.Map Coord

type PrintCell v = Grid v -> Int -> Int -> IO ()

printGrid :: PrintCell v -> Grid v -> IO ()
printGrid printF grid = do
  let coords = Map.keys grid
  let maxY = maximum $ map snd coords
  mapM_ printLine [0 .. maxY]
  putStrLn "-------"
  where
    printLine y = do
      mapM_ (printF grid y) [0 .. maximum (map fst (Map.keys grid))]
      putStrLn ""

parseInput :: (Char -> v) -> [String] -> Grid v
parseInput cellFromChar input = Map.fromList $ do
  (y, line) <- zip [0 ..] input
  (x, char) <- zip [0 ..] line
  pure ((x, y), cellFromChar char)

movementDelta :: Direction -> Coord
movementDelta North = (0, -1)
movementDelta East = (1, 0)
movementDelta South = (0, 1)
movementDelta West = (-1, 0)

moveCoord :: Coord -> Direction -> Coord
moveCoord (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = movementDelta dir
