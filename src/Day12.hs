module Day12
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.Bifunctor
import Data.Function
import Data.List (nub, nubBy)
import qualified Data.Set as S
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

parse :: String -> Grid
parse input =
  let l = lines input
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

part1 :: String -> Int
part1 input = undefined

part2 :: String -> Int
part2 input = undefined

-- allPerimiters :: Grid -> [(Pos, Char)] -> S.Set Pos -> [(Int, Int)]
-- allPerimiters grid ((pos, char) : rest) visit
--   | S.member pos visit = [] : allPerimiters grid rest visit
--   | otherwise =
--       let group = perimiter grid char pos S.empty
--        in undefined
--
-- perimiter :: Grid -> Char -> Pos -> S.Set Pos -> [(Pos, Int)]
-- perimiter grid char pos visit
--   | pos `S.member` visit = []
--   | not $ A.inRange (A.bounds grid) pos = [(pos, 1)]
--   | grid A.! pos == char = (pos, 0) : join (map rec adjacent)
--   | otherwise = [(pos, 1)]
--   where
--     rec p = perimiter grid char (addPos pos p) (S.insert pos visit)
--
-- getGroup :: Grid -> Char -> Pos -> [Pos]
-- getGroup grid char pos
--   | not $ A.inRange (A.bounds grid) pos = []
--   | (grid A.! pos) /= char = []
--   | otherwise = pos : join (map (getGroup grid char . addPos pos) adjacent)
--
-- part2 :: String -> Int
-- part2 input = undefined
--
-- surrounding :: [Pos]
-- surrounding = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1], (r, c) /= (0, 0)]
--
-- adjacent :: [Pos]
-- adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]
--
-- addPos :: Pos -> Pos -> Pos
-- addPos (a, b) (c, d) = (a + c, b + d)
