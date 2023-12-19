{-# LANGUAGE DerivingStrategies #-}

module Year2023.Day07 (solve) where

import Paths_advent_of_code (getDataFileName)
import Year2023.Day07.Part1 qualified as Part1 (parseFile, solve)
import Year2023.Day07.Part2 qualified as Part2 (parseFile, solve)

input :: String
input = "app/data/year2023/day07.txt"

solve :: IO ()
solve = do
  filename <- getDataFileName input
  file <- readFile filename
  let part1HandAndBids = Part1.parseFile file
  case part1HandAndBids of
    Right handAndBids ->
      print ("Day 7 Part 1 answer: " ++ show (Part1.solve handAndBids))
    Left _ -> print "Day 7 Part 1 error parsing file"
  let part2HandAndBids = Part2.parseFile file
  case part2HandAndBids of
    Right handAndBids ->
      print ("Day 7 Part 2 answer: " ++ show (Part2.solve handAndBids))
    Left _ -> print "Day 7 Part 2 error parsing file"
