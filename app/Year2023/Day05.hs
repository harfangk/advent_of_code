{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day05 (solve) where

import Control.Monad (void)
import Data.List qualified as List (find, sort)
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

data Category (a :: CategoryRelation)
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving stock (Show, Eq, Ord)

data CategoryRelation = Source | Destination

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

data Almanac2 = Almanac2
  { seedRanges :: [(Int, Int)],
    categoryMapRanges :: [[CategoryMapRanges]]
  }
  deriving stock (Show)

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1Almanac = parse pAlmanac "" file
  case part1Almanac of
    Right almanac ->
      print ("Day 5 Part 1 answer: " ++ show (solve' almanac))
    Left _ -> print "Error parsing almanac"
  let part2Almanac = parse pAlmanac2 "" file
  case part2Almanac of
    Right almanac ->
      print ("Day 5 Part 2 answer: " ++ show (solve'' almanac))
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
    step categoryMap ((source, destination) : rest) n = step categoryMap rest (toNextStage categoryMap (source, destination) n)

solve'' :: Almanac2 -> Int
solve'' almanac = minimum . map fst . step (seedRanges almanac) $ categoryMapRanges almanac
  where
    step :: [(Int, Int)] -> [[CategoryMapRanges]] -> [(Int, Int)]
    step inputs [] = inputs
    step inputs (cmr : cmrs) = step newInputs cmrs
      where
        newInputs = List.sort . applyMap inputs $ cmr

toNextStage :: CategoryMap -> (Category Source, Category Destination) -> Int -> Int
toNextStage categoryMap key = findAnswer ((Map.!) categoryMap key)
  where
    findAnswer :: [CategoryMapRanges] -> Int -> Int
    findAnswer ranges target' =
      case List.find (\(CategoryMapRanges {sourceStart, sourceEnd}) -> sourceStart <= target' && target' < sourceEnd) ranges of
        Just (CategoryMapRanges {sourceStart, destinationStart}) -> target' - (sourceStart - destinationStart)
        Nothing -> target'

pAlmanac :: Parser Almanac
pAlmanac = do
  seeds <- pPart1Seeds
  categoryMapRawData <- someTill pCategoryMap eof
  let map' = buildCategoryMap categoryMapRawData
  pure Almanac {seeds = seeds, categoryMaps = map'}

pAlmanac2 :: Parser Almanac2
pAlmanac2 = do
  seedRanges <- pPart2Seeds
  categoryMapRawData <- someTill pCategoryMap eof
  let categoryMapRanges = map (List.sort . map processRanges . destinationSourceRanges) categoryMapRawData
  pure Almanac2 {seedRanges = seedRanges, categoryMapRanges = categoryMapRanges}

buildCategoryMap :: [CategoryMapRawData] -> CategoryMap
buildCategoryMap = Map.fromList . map buildKVList
  where
    buildKVList :: CategoryMapRawData -> ((Category Source, Category Destination), [CategoryMapRanges])
    buildKVList CategoryMapRawData {sourceCategory, destinationCategory, destinationSourceRanges} =
      ((sourceCategory, destinationCategory), List.sort $ map processRanges destinationSourceRanges)

processRanges :: (Int, Int, Int) -> CategoryMapRanges
processRanges (destinationStart, sourceStart, range) =
  CategoryMapRanges
    { destinationStart = destinationStart,
      sourceStart = sourceStart,
      destinationEnd = destinationStart + range,
      sourceEnd = sourceStart + range
    }

pPart1Seeds :: Parser [Int]
pPart1Seeds = do
  void (string "seeds: ")
  seeds <- some digitChar `sepEndBy1` space
  space
  pure (map read seeds)

pPart2Seeds :: Parser [(Int, Int)]
pPart2Seeds = do
  void (string "seeds: ")
  seeds <- some digitChar `sepEndBy1` space
  space
  pure (toTuple . map read $ seeds)
  where
    toTuple :: [Int] -> [(Int, Int)]
    toTuple [] = []
    toTuple [_] = []
    toTuple (s : r : rest) = (s, s + r) : toTuple rest

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

applyMap :: [(Int, Int)] -> [CategoryMapRanges] -> [(Int, Int)]
applyMap inputs ranges = concatMap (step [] ranges) inputs
  where
    step :: [(Int, Int)] -> [CategoryMapRanges] -> (Int, Int) -> [(Int, Int)]
    step acc [] _input' = acc
    step acc (range' : ranges') input'@(is, ie)
      -- Case 1: End of input range lies to the left of source range
      | ie <= ss = input' : acc
      -- Case 2: Input range overlaps to the left of source range
      | is < ss && ss < ie && ie <= ss = (ds, ds + ie - ss) : acc
      -- Case 3: Input range includes the source range
      | is <= ss && se <= ie = step ((ds, de) : acc) ranges' (se, ie)
      -- Case 4: Input range lies inside the source range
      | ss <= is && ie <= se = (is + shift, ie + shift) : acc
      -- Case 5: Input range overlaps to the right of source range
      | ss <= is && is < se && se <= ie = step ((ds + is - ss, de) : acc) ranges' (se, ie)
      -- Case 6: End of input range lies to the right of source range
      | otherwise = step acc ranges' input'
      where
        shift = ds - ss
        ss = sourceStart range'
        se = sourceEnd range'
        ds = destinationStart range'
        de = destinationEnd range'
