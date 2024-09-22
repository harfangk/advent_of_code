{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day12 (solve) where

import Data.List qualified as List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map (filter, fromList, keys)
import Data.Void (Void)
import Debug.Trace (trace)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, empty, oneOf, parse, parseTest, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (char, digitChar, hspace1, newline, space)
import Text.Megaparsec.Char.Lexer qualified as L

input :: String
input = "app/data/year2023/day12-2.txt"

type Parser = Parsec Void String

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let day12Input = parse pInput filename file
  case day12Input of
    Right day12Input' -> do
      print ("Day 12 Part 1 answer: " ++ show (sum . map countArrangements $ day12Input'))
    Left _ -> print "Day 12 error parsing file"

{-
      print ("Day 12 Part 2 answer: " ++ show (solve' 1000000 day12Input'))
-}

countArrangements :: (String, [Int]) -> Int
countArrangements (record, groups) = trace ("RECORD: " ++ show record ++ ", Groups: " ++ show groups ++ ", Result: " ++ show result) result
  where
    result = doCountArrangements record groups
    isValidSegment :: String -> Int -> Bool
    isValidSegment record' group'
      | length record' < group' = False
      | length record' == group' = all (`elem` "#?") segment
      | otherwise = length . takeWhile (`elem` "#?") $ record'
      where
        segmentAndSeparator = take (group' + 1) record'
        segment = take group' segmentAndSeparator
        separator = last segmentAndSeparator
    doCountArrangements :: String -> [Int] -> Int
    doCountArrangements remRecord [] = trace ("Valid arrangement, remRecord: " ++ show remRecord) 1
    doCountArrangements [] remGroups = trace ("Invalid arrangement, remGroups: " ++ show remGroups) 0
    doCountArrangements record'@(c : cs) groups'@(i : is) =
      case c of
        '.' -> doCountArrangements (trace ("c: ., nextRecord: " ++ show cs ++ ", remGroups: " ++ show groups') cs) groups'
        '?' ->
          if isValidSegment record' i
            then doCountArrangements (trace ("c: ? to #, nextRecord: " ++ show ('#' : cs) ++ ", remGroups: " ++ show groups') ('#' : cs)) groups' + doCountArrangements (trace ("c: ? to ." ++ ", nextRecord: " ++ show cs ++ ", nextGroups: " ++ show groups') cs) groups'
            else doCountArrangements (trace ("c: ? and invalid, nextRecord: " ++ show (dropWhile (`elem` "#?") record') ++ ", remGroups: " ++ show groups') dropWhile (`elem` "#?") record') groups'
        '#' ->
          if isValidSegment record' i
            then doCountArrangements (drop (i + 1) record') (trace ("c: # and valid, nextRecord: " ++ show (drop (i + 1) record') ++ ", remGroups: " ++ show is) is)
            else doCountArrangements (trace ("c: # and invalid, nextRecord: " ++ show (dropWhile (`elem` "#?") record') ++ ", remGroups: " ++ show groups') dropWhile (`elem` "#?") record') groups'
        _ -> doCountArrangements cs groups'

sc :: Parser ()
sc = L.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

pInput :: Parser [(String, [Int])]
pInput = pRow `sepEndBy1` newline

pRow :: Parser (String, [Int])
pRow = do
  conditionRecord <- pString
  space
  contiguousDamagedSpringGroups <- pContiguousDamagedSpringGroups
  pure (conditionRecord, contiguousDamagedSpringGroups)

pString :: Parser String
pString = some (oneOf ".#?")

pContiguousDamagedSpringGroups :: Parser [Int]
pContiguousDamagedSpringGroups = integer `sepBy1` char ','
