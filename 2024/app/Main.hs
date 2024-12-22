module Main where

import Aoc1
import Aoc2
import Aoc3
import Aoc4
import Aoc5
import Aoc6
import Aoc7
import Input (parseInputForDay)

day :: String
day = "7"
test :: Bool
test = False

main :: IO ()
main = do
  runDay 

runDay :: IO ()
runDay = do
  input <- parseInputForDay day test
  case day of
    "1" -> Aoc1.solve input
    "2" -> Aoc2.solve input
    "3" -> Aoc3.solve input
    "4" -> Aoc4.solve input
    "5" -> Aoc5.solve input
    "6" -> Aoc6.solve input
    "7" -> Aoc7.solve input
    _ -> putStrLn "Invalid day"
