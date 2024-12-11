module Day11
  ( part1,
    part2,
  )
where

import Control.Monad
import Data.Array as A
import Debug.Trace
import Data.MemoTrie (memoFix)

part1 :: String -> Int
part1 input = length $ blinkN (parse input) 0

part2 :: String -> Int
part2 input = length $ blinkNMemo (parse input) 50

parse :: String -> [Int]
parse = map read . words


blinkN :: [Int] -> Int -> [Int]
blinkN nums 0 = nums
blinkN nums n = blinkN (traceShowId $ blink nums) (n - 1)

blinkNMemo :: [Int] -> Int -> [Int]
blinkNMemo nums 0 = nums
blinkNMemo nums n = blinkNMemo (traceShowId $ blink nums) (n - 1)

blink :: [Int] -> [Int]
blink [] = []
blink (0 : xs) = 1 : blink xs
blink (x : xs)
  -- | traceShow (x) False = undefined
  | even . length . show $ x =
      let num = show x
          (l, r) = splitAt (length num `div` 2) num
       in read l : read r : blink xs
  | otherwise = x * 2024 : blink xs

type Memo f = f -> f
blinkOne :: Memo (Int -> [Int])
blinkOne blinkOne 0 = [1]
blinkOne blinkOne x
  | even . length . show $ x =
      let num = show x
          (l, r) = splitAt (length num `div` 2) num
       in [read l, read r]
  | otherwise =  [x * 2024]


blinkMemo :: Int -> [Int]
blinkMemo = memoFix blinkOne

