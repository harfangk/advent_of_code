module Year2023.Day01.Solution (solve) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Paths_advent_of_code (getDataFileName)

input :: String
input = "app/data/year2023/day01.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- TIO.readFile filename
  let part1Result = sum . map getCalibrationValue1 . T.lines $ file
  print part1Result

getCalibrationValue1 :: T.Text -> Int
getCalibrationValue1 text =
  let digitText = T.filter isDigit text
   in case (T.uncons digitText, T.unsnoc digitText) of
        (Nothing, _) -> 0
        (_, Nothing) -> 0
        (Just (n, _), Just (_, m)) -> read [n, m]
