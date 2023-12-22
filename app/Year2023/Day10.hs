{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day10 (solve) where

import Control.Applicative (liftA2)
import Data.Map (Map)
import Data.Map qualified as Map (filter, fromList, keys, lookup, (!))
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
solvePart1 (sCoord, grid) = div (maybe 0 length (traverseL grid sCoord N)) 2

solvePart2 :: (Coord, Grid) -> Int
solvePart2 (sCoord, grid) = case traverseL grid sCoord N of
  Nothing -> 0
  Just path -> div (shoelace path - length path + 3) 2

connect :: Grid -> (Char -> Bool) -> Coord -> Maybe Coord
connect grid c coord = Map.lookup coord grid >>= \t -> if c t then Just coord else Nothing

connectNorth :: Char -> Bool
connectNorth = (`elem` "|F7S")

connectSouth :: Char -> Bool
connectSouth = (`elem` "|LJS")

connectEast :: Char -> Bool
connectEast = (`elem` "-7JS")

connectWest :: Char -> Bool
connectWest = (`elem` "-FLS")

traverseL :: Grid -> Coord -> Direction -> Maybe [Coord]
traverseL grid (x, y) dir = case dir of
  E ->
    connect grid connectEast (x + 1, y) >>= \c -> case grid Map.! c of
      '-' -> doTraverseL grid c E
      '7' -> doTraverseL grid c S
      'J' -> doTraverseL grid c N
      _ -> Just [c]
  W ->
    connect grid connectWest (x - 1, y) >>= \c -> case grid Map.! c of
      '-' -> doTraverseL grid c W
      'F' -> doTraverseL grid c S
      'L' -> doTraverseL grid c N
      _ -> Just [c]
  N ->
    connect grid connectNorth (x, y - 1) >>= \c -> case grid Map.! c of
      '|' -> doTraverseL grid c N
      'F' -> doTraverseL grid c E
      '7' -> doTraverseL grid c W
      _ -> Just [c]
  S ->
    connect grid connectSouth (x, y + 1) >>= \c -> case grid Map.! c of
      '|' -> doTraverseL grid c S
      'J' -> doTraverseL grid c W
      'L' -> doTraverseL grid c E
      _ -> Just [c]
  where
    doTraverseL g c d = liftA2 (:) (Just c) (traverseL g c d)

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
      | (y, row) <- zip [0 ..] rows,
        (x, tile) <- zip [0 ..] row
    ]

shoelace :: [Coord] -> Int
shoelace coords = abs . sum . zipWith (\(x, y) (x', y') -> (x - x') * (y + y')) coords $ tail coords
