module Main (main) where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day2
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> mapM_ runDay [1 .. 25]
    Just day -> runDay day

runDay :: Int -> IO ()
runDay num = do
  input <- readFile $ "data/day" ++ show num
  putStrLn $ "Day: " ++ show num
  putStrLn $ "  Part 1: " ++ show (part1 num input)
  putStrLn $ "  Part 2: " ++ show (part2 num input)

parseArgs :: [String] -> Maybe Int
parseArgs [] = Nothing
parseArgs [num] = Just $ read num
parseArgs _ = error "invalid argument, input a number between 1 and 25 or nothing."

part1 :: Int -> String -> Int
part1 1 = Day1.part1
part1 2 = Day2.part1
part1 3 = Day3.part1
part1 4 = Day4.part1
part1 5 = Day5.part1
part1 6 = Day6.part1
part1 7 = Day7.part1
part1 8 = Day8.part1
part1 9 = Day9.part1
part1 10 = Day10.part1
part1 11 = Day11.part1
part1 12 = Day12.part1
part1 13 = Day13.part1
part1 14 = Day14.part1
part1 15 = Day15.part1
part1 16 = Day16.part1
part1 17 = Day17.part1
part1 18 = Day18.part1
part1 19 = Day19.part1
part1 20 = Day20.part1
part1 21 = Day21.part1
part1 22 = Day22.part1
part1 23 = Day23.part1
part1 24 = Day24.part1
part1 25 = Day25.part1
part1 _ = error "invalid argument, input a number between 1 and 25 or nothing."

part2 :: Int -> String -> Int
part2 1 = Day1.part2
part2 2 = Day2.part2
part2 3 = Day3.part2
part2 4 = Day4.part2
part2 5 = Day5.part2
part2 6 = Day6.part2
part2 7 = Day7.part2
part2 8 = Day8.part2
part2 9 = Day9.part2
part2 10 = Day10.part2
part2 11 = Day11.part2
part2 12 = Day12.part2
part2 13 = Day13.part2
part2 14 = Day14.part2
part2 15 = Day15.part2
part2 16 = Day16.part2
part2 17 = Day17.part2
part2 18 = Day18.part2
part2 19 = Day19.part2
part2 20 = Day20.part2
part2 21 = Day21.part2
part2 22 = Day22.part2
part2 23 = Day23.part2
part2 24 = Day24.part2
part2 25 = Day25.part2
part2 _ = error "invalid argument, input a number between 1 and 25 or nothing."
