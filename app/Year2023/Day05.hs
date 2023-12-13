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

data Category a
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving stock (Show, Eq, Ord)

data Source

data Destination

data CategoryMapRawData = CategoryMapRawData
  { sourceCategory :: Category Source,
    destinationCategory :: Category Destination,
    destinationSourceRanges :: [(Int, Int, Int)]
  }
  deriving stock (Show)

type CategoryMap = Map (Category Source, Category Destination) [CategoryMapRanges]

data CategoryMapRanges = CategoryMapRanges {sourceStart :: Int, sourceEnd :: Int, destinationStart :: Int, destinationEnd :: Int} deriving stock (Show, Eq)

instance Ord CategoryMapRanges where
  cmr1 `compare` cmr2 = sourceStart cmr1 `compare` sourceStart cmr2

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
      print ("Day 5 Part 1 answer: " ++ show (solve' almanac))
    Left _ -> print "Error parsing almanac"
  let part2Almanac = parse (pAlmanac pPart2Seeds) "" file
  case part2Almanac of
    Right almanac -> print ("Day 5 Part 2 answer: " ++ show (solve' almanac))
    Left _ -> print "Error parsing almanac"

sc :: Parser ()
sc = L.space (void eol) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

solve' :: Almanac -> Int
solve' almanac = minimum . map (step (categoryMaps almanac) transformationOrder) . seeds $ almanac
  where
    transformationOrder :: [(Category Source, Category Destination)] =
      [ (Seed, Soil),
        (Soil, Fertilizer),
        (Fertilizer, Water),
        (Water, Light),
        (Light, Temperature),
        (Temperature, Humidity),
        (Humidity, Location)
      ]
    step :: CategoryMap -> [(Category Source, Category Destination)] -> Int -> Int
    step _ [] n = n
    step _ [_c] n = n
    step categoryMap ((source, destination) : rest) n = step categoryMap rest (toNextStage categoryMap (source, destination) n)

-- (Category Source a, Category Destination b) -> (Category Source b, Category Destination c) -> (Category Source a, Category Destination c)
-- type Category a b = Category
--

toNextStage :: CategoryMap -> (Category Source, Category Destination) -> Int -> Int
toNextStage categoryMap key target = findAnswer ((Map.!) categoryMap key) target
  where
    findAnswer :: [CategoryMapRanges] -> Int -> Int
    findAnswer ranges target' =
      case List.find (\(CategoryMapRanges {sourceStart, sourceEnd}) -> sourceStart <= target' && target <= sourceEnd) ranges of
        Just (CategoryMapRanges {sourceStart, destinationStart}) -> target' - (sourceStart - destinationStart)
        Nothing -> target'

pAlmanac :: Parser [Int] -> Parser Almanac
pAlmanac seedsParser = do
  seeds <- seedsParser
  categoryMapRawData <- someTill pCategoryMap eof
  let map' = buildCategoryMap categoryMapRawData
  pure Almanac {seeds = seeds, categoryMaps = map'}

buildCategoryMap :: [CategoryMapRawData] -> CategoryMap
buildCategoryMap = Map.fromList . map buildKVList
  where
    buildKVList :: CategoryMapRawData -> ((Category Source, Category Destination), [CategoryMapRanges])
    buildKVList CategoryMapRawData {sourceCategory, destinationCategory, destinationSourceRanges} =
      ((sourceCategory, destinationCategory), map processRanges destinationSourceRanges)
    processRanges :: (Int, Int, Int) -> CategoryMapRanges
    processRanges (destinationStart, sourceStart, range) =
      CategoryMapRanges
        { destinationStart = destinationStart,
          sourceStart = sourceStart,
          destinationEnd = destinationStart + range - 1,
          sourceEnd = sourceStart + range - 1
        }

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
    pCategory :: Parser (Category a)
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

mergeCategoryMaps :: CategoryMap -> CategoryMap -> CategoryMap
mergeCategoryMaps cm1 cm2 = cm1
  where
    dsrs1 = destinationSourceRanges cmrd1
    dsrs2 = destinationSourceRanges cmrd2
    newRanges = concatMap (step dsrs2) dsrs1
      where
        step :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
        step dsrs2' dsrs1' = concatMap (mapper dsrs1') dsrs2'
          where
            mapper :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
            mapper (d1, s1, r1) (d2, s2, r2)
              | s2 <= d1 && d1 + r1 < s2 + r2 = [(d2, s1, r1)]
              | s2 <= d1 = []
              | d1 < s2 + r2 = []
              | otherwise = []

-- If a subrange SubD1n within D1n overlaps with any of subranges SubS2m within S2m,
-- source of SubD1n should point to SubD2m
