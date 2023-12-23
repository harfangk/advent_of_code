{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day11 (solve) where

import Control.Applicative (liftA2)
import Data.List (subsequences)
import Data.List qualified as List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map (filter, fromList, keys, lookup, (!))
import Data.Void (Void)
import Debug.Trace (trace)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, oneOf, parse, sepEndBy1, some)
import Text.Megaparsec.Char (newline)

input :: String
input = "app/data/year2023/day11.txt"

type Parser = Parsec Void String

type Coord = (Int, Int)

type Grid = Map Coord Char

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let day11Input = parse pInput filename file
  case day11Input of
    Right day11Input' -> do
      print ("Day 11 Part 1 answer: " ++ show (solvePart1 day11Input'))
    -- print ("Day 11 Part 2 answer: " ++ show (solvePart2 day11Input'))
    Left _ -> print "Day 11 error parsing file"

solvePart1 :: (Grid, [Int], [Int]) -> Int
solvePart1 (grid, emptyRows, emptyCols) = sum . map (calculateDistance 2 emptyRows emptyCols) . subsequencesOfSize 2 . Map.keys . Map.filter (== '#') $ grid

{-

solvePart2 :: (Coord, Grid) -> Int
solvePart2 (sCoord, grid) = case traverseL grid sCoord N of
  Nothing -> 0
  Just path -> div (shoelace path - length path + 3) 2
 -}

calculateDistance :: Int -> [Int] -> [Int] -> [Coord] -> Int
calculateDistance maxAge emptyRows emptyCols [(x1, y1), (x2, y2)] = calculateDistance' maxAge emptyCols x1 x2 + calculateDistance' maxAge emptyRows y1 y2
  where
    calculateDistance' :: Int -> [Int] -> Int -> Int -> Int
    calculateDistance' maxAge' emptyIndices x1' x2' = abs (x1' - x2') + ((length . filter (\i -> min x1' x2' < i && i < max x1' x2') $ emptyIndices) * (maxAge' - 1))
calculateDistance _ _ _ _ = 0

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l
        then []
        else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs') =
      let next = subsequencesBySize xs'
       in zipWith
            (++)
            ([] : next)
            (map (map (x :)) next ++ [[]])

pInput :: Parser (Grid, [Int], [Int])
pInput = do
  rows <- pRow `sepEndBy1` newline
  let emptyRows = findEmptyRows rows
  let emptyCols = findEmptyRows (List.transpose rows)
  pure (rowsToGrid rows, emptyRows, emptyCols)

pGrid :: Parser Grid
pGrid = rowsToGrid . expand <$> pRow `sepEndBy1` newline

rowsToGrid :: [String] -> Grid
rowsToGrid rows =
  Map.fromList
    [ ((x, y), tile)
      | (y, row) <- zip [0 ..] rows,
        (x, tile) <- zip [0 ..] row
    ]

findEmptyRows :: [String] -> [Int]
findEmptyRows = map fst . filter (all (== '.') . snd) . zip [0 ..]

expand :: [String] -> [String]
expand = doExpand . List.transpose . doExpand
  where
    doExpand :: [String] -> [String]
    doExpand = reverse . foldl (\acc row -> if all (== '.') row then row : row : acc else row : acc) []

pRow :: Parser String
pRow = some (oneOf ".#")
