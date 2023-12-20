{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day08 (solve) where

import Control.Monad (void)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, (!))
import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, choice, parse, sepBy1, some)
import Text.Megaparsec.Char (char, space, upperChar)

input :: String
input = "app/data/year2023/day08.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1Data = parse pData "" file
  case part1Data of
    Right part1Data' ->
      print ("Day 8 Part 1 answer: " ++ show (solve' part1Data'))
    Left _ -> print "Day 8 Part 1 error parsing file"

data Instruction = L | R deriving stock (Show)

type Parser = Parsec Void String

type Graph = Map String (String, String)

type Node = String

solve' :: ([Instruction], Graph) -> Int
solve' (instructions, graph) = step 0 "AAA" (cycle instructions) graph
  where
    step :: Int -> Node -> [Instruction] -> Graph -> Int
    step count "ZZZ" _ _ = count
    step count currentNode (i : is) graph' = step (count + 1) (getNextNode currentNode i graph') is graph
    step count _ _ _ = count

getNextNode :: Node -> Instruction -> Graph -> Node
getNextNode currentNode instruction graph =
  case instruction of
    L -> fst currentEdge
    R -> snd currentEdge
  where
    currentEdge = graph Map.! currentNode

pData :: Parser ([Instruction], Graph)
pData = do
  instruction <- some pInstruction
  space
  graph <- pGraph
  pure (instruction, graph)

pGraph :: Parser Graph
pGraph = do
  edges <- pEdge `sepBy1` space
  let graph = Map.fromList edges
  pure graph

pInstruction :: Parser Instruction
pInstruction = choice [L <$ char 'L', R <$ char 'R']

pNode :: Parser String
pNode = some upperChar

pEdge :: Parser (Node, (Node, Node))
pEdge = do
  current <- pNode
  space
  void (char '=')
  space
  void (char '(')
  left <- pNode
  void (char ',')
  space
  right <- pNode
  void (char ')')
  space
  pure (current, (left, right))
