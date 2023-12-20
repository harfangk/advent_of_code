{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day08 (solve) where

import Control.Monad (void)
import Data.List qualified as List (filter)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (fromList, keys, (!))
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
  let day08Input = parse pData "" file
  case day08Input of
    Right day08Input' -> do
      print ("Day 8 Part 1 answer: " ++ show (solvePart1 day08Input'))
      print ("Day 8 Part 2 answer: " ++ show (solvePart2 day08Input'))
    Left _ -> print "Day 8 error parsing file"

data Instruction = L | R deriving stock (Show)

type Parser = Parsec Void String

type Graph = Map Node (Node, Node)

data Node = Node Char Char Char deriving stock (Eq, Ord, Show)

solvePart1 :: ([Instruction], Graph) -> Int
solvePart1 (instructions, graph) = step 0 (Node 'A' 'A' 'A') (cycle instructions) graph
  where
    step :: Int -> Node -> [Instruction] -> Graph -> Int
    step count (Node 'Z' 'Z' 'Z') _ _ = count
    step count currentNode (i : is) graph' = step (count + 1) (getNextNode i graph' currentNode) is graph
    step count _ _ _ = count

solvePart2 :: ([Instruction], Graph) -> Int
solvePart2 (instructions, graph) = foldl1 lcm (map (findIndividualLoop 0 (cycle instructions) graph) initialNodes)
  where
    initialNodes :: [Node] = List.filter (\(Node _ _ c) -> c == 'A') (Map.keys graph)
    findIndividualLoop :: Int -> [Instruction] -> Graph -> Node -> Int
    findIndividualLoop count _ _ (Node _ _ 'Z') = count
    findIndividualLoop count (i : is) graph' currentNode = findIndividualLoop (count + 1) is graph (getNextNode i graph' currentNode)
    findIndividualLoop count _ _ _ = count

getNextNode :: Instruction -> Graph -> Node -> Node
getNextNode instruction graph currentNode =
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

pNode :: Parser Node
pNode = do
  c1 <- upperChar
  c2 <- upperChar
  Node c1 c2 <$> upperChar

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
