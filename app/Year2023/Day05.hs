{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day05 (solve) where

import Control.Monad (void)
import Data.List qualified as List (find)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, (!))
import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (MonadParsec (eof), Parsec, choice, empty, parse, sepBy1, sepEndBy1, some, someTill)
import Text.Megaparsec.Char (digitChar, eol, hspace, space, string)
import Text.Megaparsec.Char.Lexer qualified as L (lexeme, space)

input :: String
input = "app/data/year2023/day05.txt"

type Parser = Parsec Void String

data Category
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving stock (Show, Eq, Ord)

data CategoryMapRawData = CategoryMapRawData
  { sourceCategory :: Category,
    destinationCategory :: Category,
    destinationSourceRanges :: [(Int, Int, Int)]
  }
  deriving stock (Show)

type CategoryMap = Map (Category, Category) [(Int, Int, Int)]

data Almanac = Almanac
  { seeds :: [Int],
    categoryMaps :: CategoryMap
  }
  deriving stock (Show)

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1Almanac = parse (pAlmanac pPart1Seeds) "" file
  case part1Almanac of
    Right almanac ->
      print ("Day 4 Part 1 answer: " ++ show (solve' almanac))
    Left _ -> print "Error parsing almanac"
  let part2Almanac = parse (pAlmanac pPart2Seeds) "" file
  case part2Almanac of
    Right almanac -> print ("Day 4 Part 2 answer: " ++ show (solve' almanac))
    Left _ -> print "Error parsing almanac"

sc :: Parser ()
sc = L.space (void eol) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

solve' :: Almanac -> Int
solve' almanac = minimum . map (step (categoryMaps almanac) [Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location]) . seeds $ almanac
  where
    step :: CategoryMap -> [Category] -> Int -> Int
    step _ [] n = n
    step _ [_c] n = n
    step categoryMap (source : destination : rest) n = step categoryMap (destination : rest) (toNextStage categoryMap (source, destination) n)

toNextStage :: CategoryMap -> (Category, Category) -> Int -> Int
toNextStage categoryMap key target = findAnswer ((Map.!) categoryMap key) target
  where
    findAnswer :: [(Int, Int, Int)] -> Int -> Int
    findAnswer ranges target' =
      case List.find (\(_destinationStart, sourceStart, range) -> sourceStart <= target' && target < sourceStart + range) ranges of
        Just (destinationStart', sourceStart', _range') -> target' - (sourceStart' - destinationStart')
        Nothing -> target'

pAlmanac :: Parser [Int] -> Parser Almanac
pAlmanac seedsParser = do
  seeds <- seedsParser
  categoryMapRawData <- someTill pCategoryMap eof
  let map' = Map.fromList . map (\CategoryMapRawData {sourceCategory, destinationCategory, destinationSourceRanges} -> ((sourceCategory, destinationCategory), destinationSourceRanges)) $ categoryMapRawData
  pure Almanac {seeds = seeds, categoryMaps = map'}

pPart1Seeds :: Parser [Int]
pPart1Seeds = do
  void (string "seeds: ")
  seeds <- some digitChar `sepEndBy1` space
  space
  pure (map read seeds)

pPart2Seeds :: Parser [Int]
pPart2Seeds = do
  void (string "seeds: ")
  seeds <- some digitChar `sepEndBy1` space
  space
  pure (spread . toTuple . map read $ seeds)
  where
    toTuple :: [a] -> [(a, a)]
    toTuple [] = []
    toTuple [_] = []
    toTuple (s : r : rest) = (s, r) : toTuple rest
    spread :: [(Int, Int)] -> [Int]
    spread =
      concatMap (\(start, range) -> [start .. (start + range - 1)])

pCategoryMap :: Parser CategoryMapRawData
pCategoryMap = lexeme $ do
  sourceCategory <- pCategory
  void (string "-to-")
  destinationCategory <- pCategory
  void (string " map:")
  void eol
  destinationSourceRanges <- pDestinationSourceRanges `sepEndBy1` eol
  pure CategoryMapRawData {sourceCategory = sourceCategory, destinationCategory = destinationCategory, destinationSourceRanges = destinationSourceRanges}
  where
    pCategory :: Parser Category
    pCategory =
      choice
        [ Soil <$ string "soil",
          Seed <$ string "seed",
          Fertilizer <$ string "fertilizer",
          Water <$ string "water",
          Light <$ string "light",
          Temperature <$ string "temperature",
          Humidity <$ string "humidity",
          Location <$ string "location"
        ]
    pDestinationSourceRanges :: Parser (Int, Int, Int)
    pDestinationSourceRanges = do
      [destinationStart, sourceStart, range] <- some digitChar `sepBy1` hspace
      pure (read destinationStart, read sourceStart, read range)
