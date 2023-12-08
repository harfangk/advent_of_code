{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day04 (solve) where

import Control.Monad (void)
import Data.Either (rights)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  let part2Answer = solvePart2 cards
  print $ "Day 4 Part 1 answer: " ++ show part1Answer
  print $ "Day 4 Part 2 answer: " ++ show part2Answer

solvePart1 :: [Card] -> Int
solvePart1 = sum . map calculatePoints . removeZeroMatches . map countWinningNumbers
  where
    calculatePoints :: Int -> Int
    calculatePoints n = 2 ^ (n - 1)
    removeZeroMatches :: [Int] -> [Int]
    removeZeroMatches = filter (> 0)

solvePart2 :: [Card] -> Int
solvePart2 cards = countWonCards . foldl updateCardCountMap (buildCardCountMap cards) $ cards
  where
    buildCardCountMap :: [Card] -> Map Int (Int, Card)
    buildCardCountMap cards' = Map.fromList (map ((\(count, card) -> (cardId card, (count, card))) . (1,)) cards')
    updateCardCountMap :: Map Int (Int, Card) -> Card -> Map Int (Int, Card)
    updateCardCountMap acc card =
      foldl addWonCards acc wonCardIds
      where
        currentCardCopies = fst (acc Map.! cardId card)
        winningNumbersCount = countWinningNumbers card
        wonCardIds = map (currentCardCopies,) [(cardId card + 1) .. (cardId card + winningNumbersCount)]
        addWonCards :: Map Int (Int, Card) -> (Int, Int) -> Map Int (Int, Card)
        addWonCards acc' (countToAdd, cardId) = Map.update (\(currentCount, card') -> Just (currentCount + countToAdd, card')) cardId acc'
    countWonCards :: Map Int (Int, Card) -> Int
    countWonCards = Map.foldr (\(count, _) acc' -> acc' + count) 0

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (`elem` xs) ys

countWinningNumbers :: Card -> Int
countWinningNumbers card = length $ intersect (winningNumbers card) (myNumbers card)

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
  pure Card {cardId = read cardId, winningNumbers = map read winningNumbers, myNumbers = map read myNumbers}

pSeparator :: Parser ()
pSeparator = do
  void (char '|')
  space
