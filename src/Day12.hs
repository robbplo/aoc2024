module Day12
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.Function
import Data.List (nubBy, nub)
import qualified Data.Set as S
import Debug.Trace
import Data.Bifunctor

type Pos = (Int, Int)

type Grid = A.Array Pos Char

parse :: String -> Grid
parse input =
  let l = lines input
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

part1 :: String -> Int
part1 input = allPerimiters (parse input) S.empty

allPerimiters :: Grid -> S.Set Pos -> [Int]
allPerimiters grid visit =
  let groups = nub $ map (\(i, e) -> (i, (getGroup grid e i))) (A.assocs grid)
   in traceShowId $ map (\(i, e) -> perimiter grid e i S.empty) groups

perimiter :: Grid -> Char -> Pos -> S.Set Pos -> Int
perimiter grid char pos visit
  | pos `S.member` visit = traceShow ("visit", pos) 0
  | not $ A.inRange (A.bounds grid) pos = 1
  -- \| traceShow (char, curr, pos) False = undefined
  | curr == char = sum $ traceShowId $ map rec adjacent
  | otherwise = 1
  where
    curr = grid A.! pos
    rec p = perimiter grid char (addPos pos p) (S.insert pos visit)

getGroup :: Grid -> Char -> Pos -> [Pos]
getGroup grid char pos
  | not $ A.inRange (A.bounds grid) pos = []
  | (grid A.! pos) /= char = []
  | otherwise = pos : join (map (getGroup grid char . addPos pos) adjacent)

part2 :: String -> Int
part2 input = undefined

surrounding :: [Pos]
surrounding = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1], (r, c) /= (0, 0)]

adjacent :: [Pos]
adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

addPos :: Pos -> Pos -> Pos
addPos (a, b) (c, d) = (a + c, b + d)
