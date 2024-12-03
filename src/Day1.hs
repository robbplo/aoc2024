{-# LANGUAGE TupleSections #-}

module Day1
  ( part1,
    part2,
  )
where

import Data.Function
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

part1 :: String -> Int
part1 input =
  let (left, right) = parse input
      combined = zip (sort left) (sort right)
   in sum $ map (abs . uncurry (-)) combined

parse :: String -> ([Int], [Int])
parse s = unzip $ map line $ lines s

line s =
  let parts = splitOn "   " s
   in (read $ head parts, read $ last parts)

part2 :: String -> Int
part2 input =
  let (left, right) = parse input
      charmap = M.fromListWith (+) $ map (,1) right
      frequencies = map (fromMaybe 0 . (charmap M.!?)) left
   in sum $ zipWith (*) left frequencies
