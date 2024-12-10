module Day9
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array as A
import Data.Char
import Data.List (group)
import Debug.Trace

part1 :: String -> Int
part1 input = checksum . combineArr . toArr $ parse input False 0

combine :: [String] -> [String] -> [String]
combine _ [] = []
combine [] _ = []
combine ("." : xs) (y : ys) = y : combine xs ys
combine (x : xs) ys = x : combine xs ys

combineArr :: A.Array Int String -> [String]
combineArr arr = uncurry combineArr' (A.bounds arr) arr

combineArr' :: Int -> Int -> A.Array Int String -> [String]
combineArr' l r arr
  | arr A.! r == "." = combineArr' l (r - 1) arr
  | l <= r = case arr A.! l of
      "." -> arr A.! r : combineArr' (l + 1) (r - 1) arr
      char -> char : combineArr' (l + 1) r arr
  | otherwise = []

checksum :: [String] -> Int
checksum xs = sum $ zipWith (\a b -> read a * b) xs [0 ..]

toArr :: [String] -> A.Array Int String
toArr list = A.listArray (1, length list) list

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

toArrTwo :: [String] -> A.Array Int String
toArrTwo list =
  let g = group . join $ list
   in A.listArray (1, length g) g

combineTwo :: A.Array Int String -> [String]
combineTwo arr = uncurry combineTwo' (A.bounds arr) arr

combineTwo' :: Int -> Int -> A.Array Int String -> [String]
combineTwo' l r arr
  | l > r = []
  | head (arr A.! r) == '.' = combineTwo' l (r - 1) arr
  | otherwise = case arr A.! l of
      ('.' : rest) ->
        let right = arr A.! r
            remainder = replicate (length right - length ('.' : rest)) '.'
         in if length remainder >= 0
              then right : remainder : combineTwo' 1 (r - 1) arr
              else combineTwo' (l + 1) (r - 1) arr

-- 0099811188827773336446555566..............
-- 009981118882777333644655556666667775888899

-- 00...111...2...333.44.5555.6666.777.888899
-- 9988887776666555544333211100

-- 009981118882777333644655556666667775
