module Aoc9 where

import AocBase (Main)
import qualified Data.Maybe
import Data.Maybe (isJust, isNothing)

type FileSystem = [Maybe Int]

solve :: Main
solve input = do
  let parsed = parseInput input
  -- printFs parsed
  -- printFs $ compactFs parsed
  putStrLn "Part 1"
  -- print $ part1 parsed
  putStrLn "Part 2"
  putStrLn $ "Result: " ++ show (part2 parsed)
  putStrLn "Done"

part1 :: FileSystem -> Int
part1 = calcChecksum . compactFs1

part2 :: FileSystem -> Int
part2 = calcChecksum . compactFs2

-- multiply each nodeId by its index in the filesystem and sum the result
calcChecksum :: FileSystem -> Int
calcChecksum fs = sum $ zipWith (*) [0 ..] (map (Data.Maybe.fromMaybe 0) fs)

-- for part two we move whole blocks at once
-- starting from the right, move each block into the leftmost space that fits it
compactFs2 :: FileSystem -> FileSystem
compactFs2 = reverse . concat . compactFs2' . partitionFs . reverse
  where
    compactFs2' :: [[Maybe Int]] -> [[Maybe Int]]
    compactFs2' [] = []
    compactFs2' parts = compacted
      where
        (beforeBlock, blocks) = break (all isJust) parts
        moved = moveFirstBlock blocks
        compacted = beforeBlock ++ if null moved then partitionFs (compactFs2 (concat $ tail blocks)) else head moved : partitionFs (reverse $ compactFs2 (reverse $ concat $ tail moved))


-- attempt to move the first file block to the last free space that fits it
-- we're doing it the other way around so that we can use the same logic as in part 1
moveFirstBlock :: [[Maybe Int]] -> [[Maybe Int]]
moveFirstBlock [] = []
moveFirstBlock (block : rest) = swapped
  where
    (beforeFreeSpace, firstFreeSpaceAndRest) = break (\x -> all isNothing x && length x >= length block) (reverse rest)
    adjustedSpace
      | null firstFreeSpaceAndRest = rest
      | otherwise = [replicate (length block) Nothing] ++ [replicate (length (head firstFreeSpaceAndRest) - length block) Nothing] ++ drop 1 firstFreeSpaceAndRest

    swapped
      | null firstFreeSpaceAndRest = block : rest
      | otherwise = swapFirstAndLast (block : reverse adjustedSpace) ++ reverse beforeFreeSpace

partitionFs :: FileSystem -> [[Maybe Int]]
partitionFs = init . foldr (\x acc -> if null (head acc) || x /= head (head acc) then [x] : acc else (x : head acc) : tail acc) [[]]

-- compact the filesystem by moving blocks from the end to the first free space recursively
compactFs1 :: FileSystem -> FileSystem
compactFs1 [] = []
compactFs1 fs = before ++ compactFs1 swapped ++ freeSpaceAtEnd
  where
    (before, after) = break isNothing fs
    (freeSpaceAtEnd, rest) = break isJust (reverse after)
    swapped = swapFirstAndLast (reverse rest)

swapFirstAndLast :: [a] -> [a]
swapFirstAndLast [] = []
swapFirstAndLast [x] = [x]
swapFirstAndLast (x : xs) = last xs : init xs ++ [x]

printFs :: FileSystem -> IO ()
printFs [] = putStr "\n"
printFs (x : xs) = do
  case x of
    Just i -> putStr (show i)
    Nothing -> putStr "."
  printFs xs

parseInput :: [String] -> FileSystem
parseInput = constructFileSystem 0 . head
  where
    charToInt :: Char -> Int
    charToInt c = read [c]
    constructFileSystem :: Int -> String -> FileSystem
    constructFileSystem _ [] = []
    constructFileSystem nodeId [x] = constructFileSystem nodeId [x, '0']
    constructFileSystem nodeId (blocks : freeSpace : rest) =
      replicate (charToInt blocks) (Just nodeId)
      ++ replicate (charToInt freeSpace) Nothing
      ++ constructFileSystem (nodeId + 1) rest
