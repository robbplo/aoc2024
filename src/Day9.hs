module Day9
  ( part1,
    part2,
  )
where

import Control.Monad
import Data.Char
import Debug.Trace

part1 :: String -> Int
part1 input =
  let disk = parse input False 0
      empty = traceShowId $ length . filter (== ".") $ disk
      combined = traceShowId $ combine disk $ (take empty . reverse . filter (/= ".")) disk
   in checksum combined

combine :: [String] -> [String] -> [String]
combine _ [] = []
combine [] _ = []
combine ("." : xs) (y : ys) = y : combine xs ys
combine (x : xs) ys = x : combine xs ys

checksum :: [String] -> Int
checksum xs = sum $ zipWith (\a b -> read a * b) xs [0 ..]

parse :: String -> Bool -> Int -> [String]
parse [] _ _ = []
parse ('\n' : xs) isEmpty id = parse xs isEmpty id
parse (x : xs) isEmpty id =
  let count = digitToInt x
      char = if isEmpty then "." else show id
      newId = if isEmpty then id else succ id
   in replicate count char <> parse xs (not isEmpty) newId

part2 :: String -> Int
part2 = undefined

-- 0099811188827773336446555566..............
-- 009981118882777333644655556666667775888899

-- 00...111...2...333.44.5555.6666.777.888899
-- 9988887776666555544333211100

-- 009981118882777333644655556666667775
