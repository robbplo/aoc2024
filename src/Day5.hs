module Day5
  ( part1,
    part2,
  )
where

import Data.List ((\\))
import Debug.Trace (traceShowId)
import GHC.Float

part1 :: String -> Int
part1 input = maximum $ map search (lines input)

search :: String -> Int
search input = (row * 8) + col
  where
    (row, col) = search' input (0, 127) (0, 7)

search' :: String -> (Int, Int) -> (Int, Int) -> (Int, Int)
search' [] row col = (fst row, snd col)
search' ('F' : xs) (minrow, maxrow) col = search' xs (minrow, (minrow + maxrow) `div` 2) col
search' ('B' : xs) (minrow, maxrow) col = search' xs ((minrow + maxrow + 1) `div` 2, maxrow) col
search' ('L' : xs) row (mincol, maxcol) = search' xs row (mincol, (mincol + maxcol) `div` 2)
search' ('R' : xs) row (mincol, maxcol) = search' xs row ((mincol + maxcol + 1) `div` 2, maxcol)

part2 :: String -> Int
part2 input = head $ [min .. max] \\ ids
  where
    ids = map search $ lines input
    min = minimum ids
    max = maximum ids
