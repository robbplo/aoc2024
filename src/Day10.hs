module Day10
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.Char
import Data.List (sort, nub)
import Data.MemoTrie (memoFix)
import qualified Data.Set as S
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

part1 :: String -> Int
part1 input =
  let grid = parse input
   in sum $ map (\(i, e) -> length . nub $ trailScore grid i (-1) S.empty) . filter ((== '0') . snd) $ A.assocs grid

parse :: String -> Grid
parse input =
  let l = lines input
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

trailScore :: Grid -> Pos -> Int -> S.Set Pos -> [Pos]
trailScore grid pos prev visit
  | prev == -1 = rec
  | pos `S.member` visit = traceShow ("seen", pos) []
  | not $ A.inRange (A.bounds grid) pos = traceShow ("outtabounds", pos) []
  | otherwise =
      case digitToInt $ grid A.! pos of
        9 -> [pos | prev == 8]
        other -> traceShow ("other", prev, other) $ if other == succ prev then rec else []
  where
    (r, c) = pos
    next nextPos = trailScore grid nextPos (succ prev) (S.insert pos visit)
    rec =
      join [next (r, c - 1) , next (r, c + 1) , next (r + 1, c) , next (r - 1, c)]

part2 :: String -> Int
part2 input = 
  let grid = parse input
   in sum $ map (\(i, e) -> length $ trailScore grid i (-1) S.empty) . filter ((== '0') . snd) $ A.assocs grid
