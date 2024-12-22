{-# LANGUAGE LambdaCase #-}

module Aoc4 where

import AocBase (Main)
import Data.List (elemIndices, isPrefixOf, transpose)
import Data.Universe.Helpers (diagonals)

testInput :: [String]
testInput =
  [ "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  ]

horizontal :: [String] -> [String]
horizontal = id

vertical :: [String] -> [String]
vertical = transpose

rightDiagonal :: [String] -> [String]
rightDiagonal = filter ((> 3) . length) . diagonals

leftDiagonal :: [String] -> [String]
leftDiagonal = rightDiagonal . map reverse

allPermutations :: [String] -> [String]
allPermutations input = do
  let base = concatMap ($ input) [horizontal, vertical, leftDiagonal, rightDiagonal]
  -- now reverse every element in base
  base ++ map reverse base

indicesOfSubStr :: String -> String -> [Int]
indicesOfSubStr [] _ = []
indicesOfSubStr sub str = filter (\i -> sub `isPrefixOf` drop i str) $ head sub `elemIndices` str

occurencesOf :: String -> String -> Int
occurencesOf str = length . indicesOfSubStr str

countOccurences :: String -> [String] -> Int
countOccurences str input = sum $ map (occurencesOf str) input

solve :: Main
solve input = do
  putStrLn "Part 1"
  print $ part1 input
  putStrLn "Part 2"
  part2 input

part1 :: [String] -> Int
part1 input = countOccurences "XMAS" (allPermutations input)

getKernels :: Int -> [[a]] -> [[[a]]]
getKernels k arr
  | length arr < k || length (head arr) < k = []
  | otherwise =
      [ [ [ arr !! (r + dr) !! (c + dc)
            | dr <- [0 .. k - 1],
              dc <- [0 .. k - 1]
          ]
          | c <- [0 .. length (head arr) - k]
        ]
        | r <- [0 .. length arr - k]
      ]

stringToKernel :: String -> [[Char]]
stringToKernel str
  | length str /= 9 = error "Input string must be exactly 9 characters long"
  | otherwise =
      [ [ str !! (i * 3 + j)
          | j <- [0 .. 2]
        ]
        | i <- [0 .. 2]
      ]

crosses :: [String] -> [[String]]
crosses input = do
  let size = 3
      strKernels = getKernels size input
      kernels = concatMap (map stringToKernel) strKernels
  -- now get index 0 and 2 of the first and last row, and index 1 of the middle row for each kernel
  -- as two strings
  map
    ( \case
        [[a, _, c], [_, e, _], [g, _, i]] -> [[a, e, i], [c, e, g]]
        _ -> []
    )
    kernels

part2 :: Main
part2 input = do
  print "Part 2"
  let cr = crosses input
      forward = countOccurences "MAS"
      backwards = countOccurences "SAM"
      -- now build tuples for each kernel
      allOccurences = map (\k -> forward k + backwards k) cr
      -- needs to appear 2 times for the cross to be considered valid
      validCrosses = filter (== 2) allOccurences

  print $ length validCrosses

-- print $ countOccurences "XMAS" (crosses input)
