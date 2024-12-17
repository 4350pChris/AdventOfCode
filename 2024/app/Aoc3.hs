module Aoc3 where

import Data.List (intercalate)
import Input (parseInputForDay)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  putStrLn "Day 3"
  part1
  part2

-- get all statements that look like "mul(1,2)" from the input
-- which looks like "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- do this using regex
parseMulStatements :: String -> [[String]]
parseMulStatements input = input =~ "mul\\(([[:digit:]]+),([[:digit:]]+)\\)" :: [[String]]

stmtToTuple :: [String] -> (Int, Int)
stmtToTuple stmt = (read (stmt !! 1) :: Int, read (stmt !! 2) :: Int)

parseInput :: IO [String]
parseInput = do
  parseInputForDay "3"

-- drop everything between "do()" and "don't()"
-- "do()mul(1,2)don't()mul(2,3)" -> "mul(1,2)"
-- take care that we start in an enabled state, i.e. as if we'd just seen "do()"
dropParts :: String -> String
dropParts = dropParts' True
  where
    dropParts' :: Bool -> String -> String
    dropParts' _ [] = []
    dropParts' _ ('d' : 'o' : '(' : ')' : xs) = dropParts' True xs
    dropParts' _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) = dropParts' False xs
    dropParts' False (_ : xs) = dropParts' False xs
    dropParts' True (x : xs) = x : dropParts' True xs

part1 :: IO ()
part1 = do
  input <- parseInput
  let mulStatements = concatMap parseMulStatements input
      tuples = map stmtToTuple mulStatements
      -- do multiplication, then sum up all the results
      result = sum $ map (uncurry (*)) tuples
  print result

part2 :: IO ()
part2 = do
  input <- parseInput
  -- this time we need to account for do() and don't() statements
  -- truncate the input, removing everything between don't() and do()
  -- to only get the enabled mul statements

  -- first concatenate the input strings, as we need to process it as a whole
  let enabledMulStatements = dropParts (intercalate "" input)
      mulStatements = parseMulStatements enabledMulStatements
      tuples = map stmtToTuple mulStatements
      result = sum $ map (uncurry (*)) tuples
  print result
