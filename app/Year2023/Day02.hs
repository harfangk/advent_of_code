{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day02 (solve) where

import Control.Monad (void)
import Data.Either (rights)
import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, parse, sepBy, some)
import Text.Megaparsec.Char (digitChar, lowerChar, space, string)

input :: String
input = "app/data/year2023/day02.txt"

type Parser = Parsec Void String

data Cubes
  = Blue Int
  | Red Int
  | Green Int
  deriving stock (Show)

data Round = Round {blueCubes :: Int, greenCubes :: Int, redCubes :: Int} deriving stock (Show)

data MaxCubes = MaxCubes {maxBlueCubes :: Int, maxGreenCubes :: Int, maxRedCubes :: Int} deriving stock (Show)

data Game = Game {gameId :: Int, rounds :: [Round]} deriving stock (Show)

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let maxCubes = MaxCubes {maxBlueCubes = 14, maxGreenCubes = 13, maxRedCubes = 12}
  let games = rights . map (parse pGame "") . lines $ file
  let part1Result = sum . map gameId . filter (isValidGame maxCubes) $ games
  print part1Result

isValidGame :: MaxCubes -> Game -> Bool
isValidGame maxCubes = all (isValidRound maxCubes) . rounds
  where
    isValidRound :: MaxCubes -> Round -> Bool
    isValidRound MaxCubes {maxBlueCubes, maxGreenCubes, maxRedCubes} currentRound =
      blueCubes currentRound <= maxBlueCubes && greenCubes currentRound <= maxGreenCubes && redCubes currentRound <= maxRedCubes

pGame :: Parser Game
pGame = do
  void (string "Game ")
  gameId <- some digitChar
  void (string ": ")
  rounds <- pRound `sepBy` string "; "
  pure (Game {gameId = read gameId, rounds = rounds})

pRound :: Parser Round
pRound = do
  cubes <- pCubes `sepBy` string ", "
  pure $ foldl step Round {blueCubes = 0, redCubes = 0, greenCubes = 0} cubes
  where
    step :: Round -> Cubes -> Round
    step acc cube =
      case cube of
        Blue n -> acc {blueCubes = n}
        Red n -> acc {redCubes = n}
        Green n -> acc {greenCubes = n}

pCubes :: Parser Cubes
pCubes = do
  cubeCount <- some digitChar
  space
  color <- some lowerChar
  pure $ case color of
    "blue" -> Blue (read cubeCount)
    "green" -> Green (read cubeCount)
    "red" -> Red (read cubeCount)
    _ -> Blue 0
