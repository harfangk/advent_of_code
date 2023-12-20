{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day09 (solve) where

import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, empty, parse, sepEndBy1, some)
import Text.Megaparsec.Char (hspace1, newline)
import Text.Megaparsec.Char.Lexer qualified as L

input :: String
input = "app/data/year2023/day09.txt"

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let day09Input = parse pReport filename file
  case day09Input of
    Right day09Input' -> do
      print ("Day 9 Part 1 answer: " ++ show (solvePart1 day09Input'))
      print ("Day 9 Part 2 answer: " ++ show (solvePart2 day09Input'))
    Left _ -> print "Day 9 error parsing file"

pHistory :: Parser [Int]
pHistory = some signedInteger

pReport :: Parser [[Int]]
pReport = pHistory `sepEndBy1` newline

solvePart1 :: [[Int]] -> Int
solvePart1 = sum . map (getNextValue . buildDiffLists)

solvePart2 :: [[Int]] -> Int
solvePart2 = sum . map (getPrevValue . buildDiffLists)

getNextValue :: [[Int]] -> Int
getNextValue = foldr (\x acc -> last x + acc) 0

getPrevValue :: [[Int]] -> Int
getPrevValue = foldr (\x acc -> head x - acc) 0

buildDiffLists :: [Int] -> [[Int]]
buildDiffLists = takeWhile (not . all (== 0)) . iterate buildDiffList

buildDiffList :: [Int] -> [Int]
buildDiffList [] = []
buildDiffList [_] = []
buildDiffList (x : y : zs) = (y - x) : buildDiffList (y : zs)
