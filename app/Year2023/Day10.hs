{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Year2023.Day10 (solve) where

import Control.Applicative (liftA2)
import Data.Map (Map)
import Data.Map qualified as Map (filter, fromList, keys, lookup)
import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, oneOf, parse, sepEndBy1, some)
import Text.Megaparsec.Char (newline)

input :: String
input = "app/data/year2023/day10.txt"

type Parser = Parsec Void String

type Coord = (Int, Int)

type Grid = Map Coord Char

data Direction = E | W | S | N deriving stock (Show)

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let day10Input = parse pInput filename file
  case day10Input of
    Right day10Input' -> do
      print ("Day 10 Part 1 answer: " ++ show (solvePart1 day10Input'))
      print ("Day 10 Part 2 answer: " ++ show (solvePart2 day10Input'))
    Left _ -> print "Day 10 error parsing file"

solvePart1 :: (Coord, Grid) -> Int
solvePart1 (sCoord, grid) = div (maybe 0 length (traverseLoop grid sCoord N)) 2

solvePart2 :: (Coord, Grid) -> Int
solvePart2 (sCoord, grid) = case traverseLoop grid sCoord N of
  Nothing -> 0
  Just path -> inversedPick (shoelace path) (length path)

traverseLoop :: Grid -> Coord -> Direction -> Maybe [Coord]
traverseLoop grid (x, y) nextDir =
  case nextDir of
    E ->
      Map.lookup (x + 1, y) grid >>= \case
        '-' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x + 1, y) E)
        'J' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x + 1, y) N)
        '7' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x + 1, y) S)
        _ -> Just [(x, y)]
    W ->
      Map.lookup (x - 1, y) grid >>= \case
        '-' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x - 1, y) W)
        'L' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x - 1, y) N)
        'F' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x - 1, y) S)
        _ -> Just [(x, y)]
    S ->
      Map.lookup (x, y + 1) grid >>= \case
        '|' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x, y + 1) S)
        'L' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x, y + 1) E)
        'J' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x, y + 1) W)
        _ -> Just [(x, y)]
    N ->
      Map.lookup (x, y - 1) grid >>= \case
        '|' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x, y - 1) N)
        '7' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x, y - 1) W)
        'F' -> liftA2 (:) (Just (x, y)) (traverseLoop grid (x, y - 1) E)
        _ -> Just [(x, y)]

pInput :: Parser (Coord, Grid)
pInput = do
  grid <- pGrid
  let s = head . Map.keys . Map.filter (== 'S') $ grid
  pure (s, grid)

pGrid :: Parser Grid
pGrid = rowsToGrid <$> pRow `sepEndBy1` newline

pRow :: Parser String
pRow = some (oneOf "|-LJ7F.S")

rowsToGrid :: [String] -> Grid
rowsToGrid rows =
  Map.fromList
    [ ((x, y), tile)
    | (y, row) <- zip [0 ..] rows
    , (x, tile) <- zip [0 ..] row
    ]

shoelace :: [Coord] -> Int
shoelace coords = div (abs . sum . zipWith (\(x, y) (x', y') -> (x - x') * (y + y')) coords $ tail coords) 2

inversedPick :: Int -> Int -> Int
inversedPick area boundaryPoints = area - div boundaryPoints 2 + 1
