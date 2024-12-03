module Day9
  ( part1,
    part1',
    part2,
    part2',
  )
where

import Control.Monad
import Data.List (tails)
import Debug.Trace

part1 :: String -> Int
part1 = part1' 25

part1' :: Int -> String -> Int
part1' size = snd . head . filter isValid . pairs . windows size . parse

parse :: String -> [Int]
parse = map read . lines

windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

pairs :: [[a]] -> [([a], a)]
pairs groups = zip groups (map last $ drop 1 groups)

isValid :: (Num a, Eq a) => ([a], a) -> Bool
isValid (window, number) = null combinations
  where
    combinations =
      [ a + b
        | a <- window,
          b <- window,
          a /= b,
          a + b == number
      ]

part2 :: String -> Int
part2 = part2' 25

part2' :: Int -> String -> Int
part2' size input = minimum result + maximum result
  where
    p1 = part1' size input
    windows = allWindows $ parse input
    result = head $ filter (\x -> sum x == p1) windows

allWindows :: [a] -> [[a]]
allWindows input = join $ map (`windows` input) [2 .. length input]
