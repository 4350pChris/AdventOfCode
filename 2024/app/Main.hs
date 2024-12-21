module Main where

import Aoc1
import Aoc2
import Aoc3
import Aoc4
import Aoc5
import Aoc6
import Input (parseInputForDay)

day :: String
day = "6"
test :: Bool
test = True

main :: IO ()
main = do
  runDay 

runDay :: IO ()
runDay = do
  input <- parseInputForDay day test
  case day of
    "1" -> Aoc1.main input
    "2" -> Aoc2.main input
    "3" -> Aoc3.main input
    "4" -> Aoc4.main input
    "5" -> Aoc5.main input
    "6" -> Aoc6.main input
    _ -> putStrLn "Invalid day"
