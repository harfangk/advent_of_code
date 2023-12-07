{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Year2023.Day03 (solve) where

import Data.Char (digitToInt, isDigit)
import Paths_advent_of_code (getDataFileName)

input :: String
input = "app/data/year2023/day03.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let linesWithIndices = addIndex . lines $ file
  let part1SymbolIndicesAndNumbers = foldl (getSymbolIndicesAndNumbers part1Symbols) ([], []) linesWithIndices
  let part1Answer = sum . map value . filterPartNumbers $ part1SymbolIndicesAndNumbers
  print $ "Day 3 Part 1 answer: " ++ show part1Answer
  let part2SymbolIndicesAndNumbers = foldl (getSymbolIndicesAndNumbers part2Symbols) ([], []) linesWithIndices
  let part2Answer = sum . map (\(_, numbers) -> product . map value $ numbers) . filter (\(_, numbers) -> length numbers == 2) . getNumbersAdjacentToGears $ part2SymbolIndicesAndNumbers
  print $ "Day 3 Part 2 answer: " ++ show part2Answer

type Index = (Int, Int)

data Number = Number {value :: Int, startIndex :: Index, endIndex :: Index} deriving stock (Show)

addIndex :: [a] -> [(Int, a)]
addIndex = zip [0 ..]

doesIntersect :: (Eq a) => [a] -> [a] -> Bool
doesIntersect x y = not . null $ (x `intersect` y)

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (`elem` xs) ys

getNumbersAdjacentToGears :: ([Index], [Number]) -> [(Index, [Number])]
getNumbersAdjacentToGears (gearIndices, numbers) =
  foldl step [] gearIndices
  where
    step :: [(Index, [Number])] -> Index -> [(Index, [Number])]
    step acc gearIndex = getAdjacentNumbers gearIndex numbers : acc
      where
        getAdjacentNumbers :: Index -> [Number] -> (Index, [Number])
        getAdjacentNumbers gearIndex' numbers' =
          (gearIndex', filter (doesIntersect (getAdjacentIndices gearIndex') . getNumberIndices) numbers')

filterPartNumbers :: ([Index], [Number]) -> [Number]
filterPartNumbers (symbolIndices, numbers) =
  filter (\number -> doesIntersect (getNumberIndices number) indicesAdjacentToSymbols) numbers
  where
    indicesAdjacentToSymbols = concatMap getAdjacentIndices symbolIndices

part1Symbols :: String
part1Symbols = "+=-@#$%&*/"

part2Symbols :: String
part2Symbols = "*"

getNumberIndices :: Number -> [Index]
getNumberIndices Number {startIndex, endIndex} =
  map (rowIdx,) colIndices
  where
    rowIdx = fst startIndex
    colIndices = [snd startIndex .. snd endIndex]

getAdjacentIndices :: Index -> [Index]
getAdjacentIndices (rowIdx, colIdx) =
  [ (rowIdx, colIdx),
    (rowIdx, colIdx + 1),
    (rowIdx, colIdx - 1),
    (rowIdx + 1, colIdx),
    (rowIdx + 1, colIdx + 1),
    (rowIdx + 1, colIdx - 1),
    (rowIdx - 1, colIdx),
    (rowIdx - 1, colIdx + 1),
    (rowIdx - 1, colIdx - 1)
  ]

getSymbolIndicesAndNumbers :: String -> ([Index], [Number]) -> (Int, String) -> ([Index], [Number])
getSymbolIndicesAndNumbers symbols acc (rowIdx, line) =
  fst (step (acc, []) (addIndex line))
  where
    step :: (([Index], [Number]), [(Int, Char)]) -> [(Int, Char)] -> (([Index], [Number]), [(Int, Char)])
    step ((symbolIndices, numbers), tempNumStorage) [] = ((symbolIndices, buildNumber rowIdx tempNumStorage ++ numbers), [])
    step ((symbolIndices, numbers), tempNumStorage) ((charIndex, char) : chars)
      | isDigit char = step ((symbolIndices, numbers), (charIndex, char) : tempNumStorage) chars
      | char `elem` symbols = step (((rowIdx, charIndex) : symbolIndices, buildNumber rowIdx tempNumStorage ++ numbers), []) chars
      | otherwise = step ((symbolIndices, buildNumber rowIdx tempNumStorage ++ numbers), []) chars

buildNumber :: Int -> [(Int, Char)] -> [Number]
buildNumber _ [] = []
buildNumber rowIdx [(idx, digit)] = [(Number {value = digitToInt digit, startIndex = (rowIdx, idx), endIndex = (rowIdx, idx)})]
buildNumber rowIdx digitsWithIndex =
  [Number {value = value, startIndex = (rowIdx, startIndex), endIndex = (rowIdx, endIndex)}]
  where
    (indices, chars) = unzip . reverse $ digitsWithIndex
    startIndex = minimum indices
    endIndex = maximum indices
    value = read chars
