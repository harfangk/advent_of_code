module Year2023.Day01.Solution (solve) where

import Data.Char (digitToInt, isDigit)
import Paths_advent_of_code (getDataFileName)

input :: String
input = "app/data/year2023/day01.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1Result = sum . map getCalibrationValue1 . lines $ file
  print part1Result

getCalibrationValue1 :: String -> Int
getCalibrationValue1 text =
  let digitText = map digitToInt . filter isDigit $ text
   in case digitText of
        [] -> 0
        [n] -> n * 10 + n
        (n : ns) -> n * 10 + last ns
