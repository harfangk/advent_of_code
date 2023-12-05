module Year2023.Day01.Solution (solve) where

import Data.Char (digitToInt, isDigit)
import Paths_advent_of_code (getDataFileName)

input :: String
input = "app/data/year2023/day01.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1Result = sum . map (getCalibrationValue . getDigits1) . lines $ file
  print part1Result
  let part2Result = sum . map (getCalibrationValue . getDigits2) . lines $ file
  print part2Result

getDigits1 :: String -> [Int]
getDigits1 = map digitToInt . filter isDigit

getDigits2 :: String -> [Int]
getDigits2 = reverse . step []
  where
    step :: [Int] -> String -> [Int]
    step acc [] = acc
    step acc text =
      case text of
        ('o' : 'n' : 'e' : _cs) -> step (1 : acc) (tail text)
        ('t' : 'w' : 'o' : _cs) -> step (2 : acc) (tail text)
        ('t' : 'h' : 'r' : 'e' : 'e' : _cs) -> step (3 : acc) (tail text)
        ('f' : 'o' : 'u' : 'r' : _cs) -> step (4 : acc) (tail text)
        ('f' : 'i' : 'v' : 'e' : _cs) -> step (5 : acc) (tail text)
        ('s' : 'i' : 'x' : _cs) -> step (6 : acc) (tail text)
        ('s' : 'e' : 'v' : 'e' : 'n' : _cs) -> step (7 : acc) (tail text)
        ('e' : 'i' : 'g' : 'h' : 't' : _cs) -> step (8 : acc) (tail text)
        ('n' : 'i' : 'n' : 'e' : _cs) -> step (9 : acc) (tail text)
        (c : cs) ->
          if isDigit c
            then step (digitToInt c : acc) cs
            else step acc cs

getCalibrationValue :: [Int] -> Int
getCalibrationValue [] = 0
getCalibrationValue [n] = n * 10 + n
getCalibrationValue (n : ns) = n * 10 + last ns
