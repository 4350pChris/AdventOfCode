module Main where

import Aoc1
import Aoc2
import Aoc3
import Input (parseInputForDay)

day :: String
day = "1"

main :: IO ()
main = do
  runDay 

runDay :: IO ()
runDay = do
  input <- parseInputForDay day
  case day of
    "1" -> Aoc1.main input
    "2" -> Aoc2.main input
    "3" -> Aoc3.main input
    _ -> putStrLn "Invalid day"
