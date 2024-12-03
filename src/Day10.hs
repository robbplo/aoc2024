module Day10
  ( part1,
    part2,
  )
where

import Data.MemoTrie (memoFix)
import Data.List (sort)
import Debug.Trace (traceShowId)

part1 :: String -> Int
part1 input = ones * threes
  where
    count n = length . filter (== n) . differences . parse $ input
    ones = count 1
    threes = 1 + count 3

parse :: String -> [Int]
parse = (0 :) . sort . map read . lines

differences :: [Int] -> [Int]
differences list = zipWith (flip (-)) list (drop 1 list)

part2 :: String -> Int
part2 = optionsMemo . parse

optionsMemo :: [Int] -> Int
optionsMemo = memoFix options

type Memo f = f -> f
options :: Memo ([Int] -> Int)
options options [_] = 1
options options (x : xs) = sum $ map (\n -> options $ drop n xs) toDrop
  where
    allowed = filter ((<= 3) . flip (-) x) $ take 3 xs
    toDrop = take (length allowed) $ iterate succ 0
