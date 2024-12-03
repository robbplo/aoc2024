{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( part1,
    part2,
  )
where

import Data.Array
import Data.List (delete)
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace

part1 :: String -> Int
part1 input = length $ filter isSafe $ parse input

parse :: String -> [[Int]]
parse = map (map read . splitOn " ") . lines

isSafe :: [Int] -> Bool
isSafe nums =
  let zipped = zipWith (-) nums (tail nums)
      monotonic = all (> 0) zipped || all (< 0) zipped
   in monotonic && all (inRange (1, 3) . abs) zipped

part2 :: String -> Int
part2 input = length $ filter maybeSafe $ parse input

maybeSafe :: [Int] -> Bool
maybeSafe nums =
  let res = any isSafe $ [delete x nums | x <- nums]
   in trace (show res <> show nums) res || isSafe nums