module Day3
  ( part1,
    part2,
  )
where

import Data.List.Split
import Debug.Trace (traceShowId)
import Text.Regex.Posix

part1 :: String -> Int
part1 input =
  let tuples = parse input
   in sum $ map (uncurry (*)) tuples

parse :: String -> [(Int, Int)]
parse input =
  let matches = getAllTextMatches $ input =~ "mul\\([0-9]+,[0-9]+\\)"
   in map toTuple matches

toTuple :: String -> (Int, Int)
toTuple mul =
  let parts = splitOn "," . init . drop 4 $ mul
   in (read . head $ parts, read . last $ parts)

part2 :: String -> Int
part2 input =
  let tuples = parse2 input
   in sum $ map (uncurry (*)) tuples

parse2 :: String -> [(Int, Int)]
parse2 input =
  let matches = traceShowId $ getAllTextMatches $ input =~ "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"
   in map toTuple $ traceShowId $ filterMatches True matches

filterMatches :: Bool -> [String] -> [String]
filterMatches _ [] = []
filterMatches _ ("do()":rest) = filterMatches True rest
filterMatches _ ("don't()":rest) = filterMatches False rest
filterMatches True (mul:rest) = mul : filterMatches True rest
filterMatches False (_:rest) = filterMatches False rest
