{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day07.Part2 (solve, parseFile) where

import Data.List qualified as List (group, sort)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, parse, sepEndBy1, some)
import Text.Megaparsec qualified as Text.MegaParsec
import Text.Megaparsec.Char (char, digitChar, space)

type Parser = Parsec Void String

data Card
  = J
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | Q
  | K
  | A
  deriving stock (Eq, Ord)

instance Show Card where
  show A = "A"
  show K = "K"
  show Q = "Q"
  show J = "J"
  show T = "T"
  show Nine = "9"
  show Eight = "8"
  show Seven = "7"
  show Six = "6"
  show Five = "5"
  show Four = "4"
  show Three = "3"
  show Two = "2"

data Hand
  = FiveOfAKind Card Card Card Card Card
  | FourOfAKind Card Card Card Card Card
  | FullHouse Card Card Card Card Card
  | ThreeOfAKind Card Card Card Card Card
  | TwoPair Card Card Card Card Card
  | OnePair Card Card Card Card Card
  | HighCard Card Card Card Card Card
  deriving stock (Eq, Show)

instance Ord Hand where
  FiveOfAKind a1 a2 a3 a4 a5 `compare` FiveOfAKind b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  FiveOfAKind {} `compare` _ = GT
  FourOfAKind {} `compare` FiveOfAKind {} = LT
  FourOfAKind a1 a2 a3 a4 a5 `compare` FourOfAKind b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  FourOfAKind {} `compare` FullHouse {} = GT
  FourOfAKind {} `compare` ThreeOfAKind {} = GT
  FourOfAKind {} `compare` TwoPair {} = GT
  FourOfAKind {} `compare` OnePair {} = GT
  FourOfAKind {} `compare` HighCard {} = GT
  FullHouse {} `compare` FiveOfAKind {} = LT
  FullHouse {} `compare` FourOfAKind {} = LT
  FullHouse a1 a2 a3 a4 a5 `compare` FullHouse b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  FullHouse {} `compare` ThreeOfAKind {} = GT
  FullHouse {} `compare` TwoPair {} = GT
  FullHouse {} `compare` OnePair {} = GT
  FullHouse {} `compare` HighCard {} = GT
  ThreeOfAKind {} `compare` FiveOfAKind {} = LT
  ThreeOfAKind {} `compare` FourOfAKind {} = LT
  ThreeOfAKind {} `compare` FullHouse {} = LT
  ThreeOfAKind a1 a2 a3 a4 a5 `compare` ThreeOfAKind b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  ThreeOfAKind {} `compare` TwoPair {} = GT
  ThreeOfAKind {} `compare` OnePair {} = GT
  ThreeOfAKind {} `compare` HighCard {} = GT
  TwoPair {} `compare` FiveOfAKind {} = LT
  TwoPair {} `compare` FourOfAKind {} = LT
  TwoPair {} `compare` FullHouse {} = LT
  TwoPair {} `compare` ThreeOfAKind {} = LT
  TwoPair a1 a2 a3 a4 a5 `compare` TwoPair b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  TwoPair {} `compare` OnePair {} = GT
  TwoPair {} `compare` HighCard {} = GT
  OnePair {} `compare` FiveOfAKind {} = LT
  OnePair {} `compare` FourOfAKind {} = LT
  OnePair {} `compare` FullHouse {} = LT
  OnePair {} `compare` ThreeOfAKind {} = LT
  OnePair {} `compare` TwoPair {} = LT
  OnePair a1 a2 a3 a4 a5 `compare` OnePair b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  OnePair {} `compare` HighCard {} = GT
  HighCard a1 a2 a3 a4 a5 `compare` HighCard b1 b2 b3 b4 b5 = a1 `compare` b1 <> a2 `compare` b2 <> a3 `compare` b3 <> a4 `compare` b4 <> a5 `compare` b5
  HighCard {} `compare` _ = LT

data HandAndBid = HandAndBid Hand Int
  deriving stock (Eq, Show)

instance Ord HandAndBid where
  HandAndBid h1 _b1 `compare` HandAndBid h2 _b2 = h1 `compare` h2

solve :: [HandAndBid] -> Int
solve handAndBids = foldl (\acc (rank, HandAndBid _ bid) -> acc + (rank * bid)) 0 (zip [1 ..] (List.sort handAndBids))

parseFile :: String -> Either (Text.MegaParsec.ParseErrorBundle String Void) [HandAndBid]
parseFile = parse (pHandAndBid `sepEndBy1` space) ""

pCard :: Parser Card
pCard =
  choice
    [ Two <$ char '2',
      Three <$ char '3',
      Four <$ char '4',
      Five <$ char '5',
      Six <$ char '6',
      Seven <$ char '7',
      Eight <$ char '8',
      Nine <$ char '9',
      T <$ char 'T',
      J <$ char 'J',
      Q <$ char 'Q',
      K <$ char 'K',
      A <$ char 'A'
    ]

pHand :: Parser Hand
pHand = do
  c1 <- pCard
  c2 <- pCard
  c3 <- pCard
  c4 <- pCard
  getHand c1 c2 c3 c4 <$> pCard

pHandAndBid :: Parser HandAndBid
pHandAndBid = do
  hand <- pHand
  space
  bid <- some digitChar
  pure $ HandAndBid hand (read bid)

getHand :: Card -> Card -> Card -> Card -> Card -> Hand
getHand c1 c2 c3 c4 c5 = applyJ jCount (getHand' c1 c2 c3 c4 c5)
  where
    cards = [c1, c2, c3, c4, c5]
    jCount = length . filter (== J) $ cards
    getHand' :: Card -> Card -> Card -> Card -> Card -> Hand
    getHand' c1' c2' c3' c4' c5' =
      case List.group . List.sort $ cards of
        [_] -> FiveOfAKind c1' c2' c3' c4' c5'
        [a, b] -> case (length a, length b) of
          (1, 4) -> FourOfAKind c1' c2' c3' c4' c5'
          (2, 3) -> FullHouse c1' c2' c3' c4' c5'
          (3, 2) -> FullHouse c1' c2' c3' c4' c5'
          (4, 1) -> FourOfAKind c1' c2' c3' c4' c5'
          _ -> HighCard c1' c2' c3' c4' c5'
        [a, b, c] -> case (length a, length b, length c) of
          (3, 1, 1) -> ThreeOfAKind c1' c2' c3' c4' c5'
          (2, 2, 1) -> TwoPair c1' c2' c3' c4' c5'
          (2, 1, 2) -> TwoPair c1' c2' c3' c4' c5'
          (1, 3, 1) -> ThreeOfAKind c1' c2' c3' c4' c5'
          (1, 1, 3) -> ThreeOfAKind c1' c2' c3' c4' c5'
          (1, 2, 2) -> TwoPair c1' c2' c3' c4' c5'
          _ -> HighCard c1' c2' c3' c4' c5'
        [_, _, _, _] -> OnePair c1' c2' c3' c4' c5'
        [_, _, _, _, _] -> HighCard c1' c2' c3' c4' c5'
        _ -> HighCard c1' c2' c3' c4' c5'
    applyJ :: Int -> Hand -> Hand
    applyJ jCount' hand =
      case (jCount', hand) of
        (4, FourOfAKind c1' c2' c3' c4' c5') -> FiveOfAKind c1' c2' c3' c4' c5'
        (3, ThreeOfAKind c1' c2' c3' c4' c5') -> FourOfAKind c1' c2' c3' c4' c5'
        (3, FullHouse c1' c2' c3' c4' c5') -> FiveOfAKind c1' c2' c3' c4' c5'
        (2, FullHouse c1' c2' c3' c4' c5') -> FiveOfAKind c1' c2' c3' c4' c5'
        (2, TwoPair c1' c2' c3' c4' c5') -> FourOfAKind c1' c2' c3' c4' c5'
        (2, OnePair c1' c2' c3' c4' c5') -> ThreeOfAKind c1' c2' c3' c4' c5'
        (1, FourOfAKind c1' c2' c3' c4' c5') -> FiveOfAKind c1' c2' c3' c4' c5'
        (1, ThreeOfAKind c1' c2' c3' c4' c5') -> FourOfAKind c1' c2' c3' c4' c5'
        (1, TwoPair c1' c2' c3' c4' c5') -> FullHouse c1' c2' c3' c4' c5'
        (1, OnePair c1' c2' c3' c4' c5') -> ThreeOfAKind c1' c2' c3' c4' c5'
        (1, HighCard c1' c2' c3' c4' c5') -> OnePair c1' c2' c3' c4' c5'
        _ -> hand
