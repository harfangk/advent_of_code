{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day04 (solve) where

import Control.Monad (void)
import Data.Either (rights)
import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, parse, sepBy, sepEndBy1, some)
import Text.Megaparsec.Char (char, digitChar, space, string)

input :: String
input = "app/data/year2023/day04.txt"

type Parser = Parsec Void String

data Card = Card {cardId :: Int, winningNumbers :: [Int], myNumbers :: [Int]} deriving stock (Show)

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let cards = parseCards . lines $ file
  let part1Answer = solvePart1 cards
  print $ "Day 4 Part 1 answer: " ++ show part1Answer

solvePart1 :: [Card] -> Int
solvePart1 = sum . map calculatePoints . removeZeroMatches . countWinningNumbers
 where
  calculatePoints :: Int -> Int
  calculatePoints n = 2 ^ (n - 1)
  removeZeroMatches :: [Int] -> [Int]
  removeZeroMatches = filter (> 0)
  countWinningNumbers :: [Card] -> [Int]
  countWinningNumbers = map (\card -> length $ intersect (winningNumbers card) (myNumbers card))
  intersect :: (Eq a) => [a] -> [a] -> [a]
  intersect [] _ = []
  intersect _ [] = []
  intersect xs ys = filter (`elem` xs) ys

parseCards :: [String] -> [Card]
parseCards = rights . map (parse pCard "")

pCard :: Parser Card
pCard = do
  void (string "Card")
  space
  cardId <- some digitChar
  void (char ':')
  space
  [winningNumbers, myNumbers] <- (some digitChar `sepEndBy1` space) `sepBy` pSeparator
  pure Card{cardId = read cardId, winningNumbers = map read winningNumbers, myNumbers = map read myNumbers}

pSeparator :: Parser ()
pSeparator = do
  void (char '|')
  space
