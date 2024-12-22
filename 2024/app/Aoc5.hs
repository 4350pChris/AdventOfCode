{-# LANGUAGE LambdaCase #-}

module Aoc5 where

import AocBase (Main)
import Data.List (elemIndex)
import Data.Map (Map, empty, insertWith, lookup)
import Data.Maybe (isNothing)

type RuleMap = Map Int [Int]

solve :: Main
solve input = do
  let (rules, pages) = parseInput input
      ruleMap = rulesToMap rules
  putStrLn "Part 1"
  print $ part1 ruleMap pages
  putStrLn "Part 2"
  print $ sumMiddleElements $ part2 ruleMap pages

part1 :: RuleMap -> [[Int]] -> Int
part1 rules pages = sumMiddleElements $ filter (isValidPage rules) pages

part2 :: RuleMap -> [[Int]] -> [[Int]]
part2 rules pages = do
  let invalidPages = filter (not . isValidPage rules) pages
  map (fixPage rules) invalidPages

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs =
  let elemI = xs !! i
      elemJ = xs !! j
      left = take i xs
      middle = take (j - i - 1) (drop (i + 1) xs)
      right = drop (j + 1) xs
   in left ++ [elemJ] ++ middle ++ [elemI] ++ right

-- fix invalid page by reordering the numbers according to the rules
-- to do this, we switch the positions of two numbers that are in violation of a rule
fixPage :: RuleMap -> [Int] -> [Int]
fixPage rules page = do
  -- first element from page for which a rule is violated
  case firstViolation rules page of
    Nothing -> page
    Just (i, j) -> fixPage rules (swapElementsAt i j page)

sumMiddleElements :: [[Int]] -> Int
sumMiddleElements = sum . map (\p -> p !! (length p `div` 2))

isValidPage :: RuleMap -> [Int] -> Bool
isValidPage rules page = isNothing (firstViolation rules page)

-- for each element in the page, check if it is in the list of forbidden followers
firstViolation :: RuleMap -> [Int] -> Maybe (Int, Int)
firstViolation rules page = firstViolation' rules page 0

-- lazily check each element in the page for a violation of the rules
-- if we find one, return the index of the item we're looking at and the index of the first forbidden follower
firstViolation' :: RuleMap -> [Int] -> Int -> Maybe (Int, Int)
firstViolation' _ [] _ = Nothing
firstViolation' rules (x : xs) i =
  maybe
    (firstViolation' rules xs (i + 1))
    ( \forbidden ->
        maybe
          (firstViolation' rules xs (i + 1))
          (\j -> Just (i, i + j + 1))
          (firstForbiddenElement forbidden xs)
    )
    (Data.Map.lookup x rules)

-- index of the first element that may not appear
firstForbiddenElement :: [Int] -> [Int] -> Maybe Int
firstForbiddenElement forbidden search = elemIndex True $ map (`elem` forbidden) search

-- convert the list of rules like "35|22" into a map that maps the second number to a list of first numbers
-- that way we get our forbidden followers for each number
rulesToMap :: [(Int, Int)] -> RuleMap
rulesToMap = foldr (\(a, b) -> insertWith (++) b [a]) empty

-- parsing

parseInput :: [String] -> ([(Int, Int)], [[Int]])
parseInput input = (parseRules input, parseUpdatePages input)

-- rules come first in our input and are recognized by the presence of a pipe "|"
-- they consist of two numbers, like 35|22 which we parse into a tuple (35, 22)
parseRules :: [String] -> [(Int, Int)]
parseRules = map parseRule . filter (elem '|')
  where
    -- split string at pipe, then parse the first and second number
    parseRule :: String -> (Int, Int)
    parseRule =
      ( \case
          [a, b] -> (read a, read b)
          _ -> error "Invalid rule format"
      )
        . words
        . map (\c -> if c == '|' then ' ' else c)

-- update pages contain numbers separated by commas
-- we parse them into a list of integers
parseUpdatePages :: [String] -> [[Int]]
parseUpdatePages = map (map read . words . map (\c -> if c == ',' then ' ' else c)) . filter (elem ',')
