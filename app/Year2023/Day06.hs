module Year2023.Day06 (solve) where

import Paths_advent_of_code (getDataFileName)

input :: String
input = "app/data/year2023/day06.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let parsedPart1Input = parsePart1Input file
  let part1Answer = solvePart1 parsedPart1Input
  print ("Day 6 Part 1 answer: " ++ show part1Answer)
  let parsedPart2Input = parsePart2Input file
  print parsedPart2Input
  let part2Answer = solvePart2 parsedPart2Input
  print ("Day 6 Part 2 answer: " ++ show part2Answer)

parsePart1Input :: String -> [(Int, Int)]
parsePart1Input = zip' . map (map read . tail . words) . lines
  where
    zip' :: [[a]] -> [(a, a)]
    zip' [] = []
    zip' [_] = []
    zip' (times : distances : rest) = zip times distances ++ zip' rest

solvePart1 :: [(Int, Int)] -> Int
solvePart1 = foldl step 1
  where
    step :: Int -> (Int, Int) -> Int
    step acc (time, distance) = acc * (length . filter (> distance) . map (\n -> n * (time - n)) $ [1 .. (time - 1)])

parsePart2Input :: String -> (Integer, Integer)
parsePart2Input = head . zip' . map (read . foldl1 (++) . tail . words) . lines
  where
    zip' :: [a] -> [(a, a)]
    zip' [] = []
    zip' [_] = []
    zip' (x : y : rest) = (x, y) : zip' rest

solvePart2 :: (Integer, Integer) -> Int
solvePart2 (time, distance) = length . filter (> distance) . map (\n -> n * (time - n)) $ [1 .. (time - 1)]
