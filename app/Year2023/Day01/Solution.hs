{-# LANGUAGE ViewPatterns #-}

module Year2023.Day01.Solution (solve) where

import Data.Char (digitToInt, isDigit)
import Data.List (stripPrefix)
import Paths_advent_of_code (getDataFileName)

input :: String
input = "app/data/year2023/day01.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1Result = sum . map (getCalibrationValue . getDigits) . lines $ file
  print part1Result
  let part2Result = sum . map getDigits2 . lines $ file
  print part2Result

getDigits :: String -> [Int]
getDigits = map digitToInt . filter isDigit

getCalibrationValue :: [Int] -> Int
getCalibrationValue [] = 0
getCalibrationValue [n] = n * 10 + n
getCalibrationValue (n : ns) = n * 10 + last ns

getDigits2 :: String -> Int
getDigits2 text = getFirstDigit text * 10 + (getSecondDigit . reverse $ text)
  where
    getFirstDigit :: String -> Int
    getFirstDigit [] = 0
    getFirstDigit (stripPrefix "one" -> Just _rest) = 1
    getFirstDigit (stripPrefix "two" -> Just _rest) = 2
    getFirstDigit (stripPrefix "three" -> Just _rest) = 3
    getFirstDigit (stripPrefix "four" -> Just _rest) = 4
    getFirstDigit (stripPrefix "five" -> Just _rest) = 5
    getFirstDigit (stripPrefix "six" -> Just _rest) = 6
    getFirstDigit (stripPrefix "seven" -> Just _rest) = 7
    getFirstDigit (stripPrefix "eight" -> Just _rest) = 8
    getFirstDigit (stripPrefix "nine" -> Just _rest) = 9
    getFirstDigit (c : cs) =
      if isDigit c
        then digitToInt c
        else getFirstDigit cs

    getSecondDigit :: String -> Int
    getSecondDigit [] = 0
    getSecondDigit (stripPrefix "eno" -> Just _rest) = 1
    getSecondDigit (stripPrefix "owt" -> Just _rest) = 2
    getSecondDigit (stripPrefix "eerht" -> Just _rest) = 3
    getSecondDigit (stripPrefix "ruof" -> Just _rest) = 4
    getSecondDigit (stripPrefix "evif" -> Just _rest) = 5
    getSecondDigit (stripPrefix "xis" -> Just _rest) = 6
    getSecondDigit (stripPrefix "neves" -> Just _rest) = 7
    getSecondDigit (stripPrefix "thgie" -> Just _rest) = 8
    getSecondDigit (stripPrefix "enin" -> Just _rest) = 9
    getSecondDigit (c : cs) =
      if isDigit c
        then digitToInt c
        else getSecondDigit cs
