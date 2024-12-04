module Day4
  ( part1,
    part2,
  )
where

import Control.Monad (join)
import qualified Data.Array as A
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

part1 :: String -> Int
part1 = dfs . parse

parse :: String -> Grid
parse input =
  let l = lines input
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

dfs :: Grid -> Int
dfs grid =
  let words = join . map wordIndices $ A.indices grid
   in length $ filter (isXmas grid) words

isXmas :: Grid -> [Pos] -> Bool
isXmas grid indices =
  let inRange = all (A.inRange (A.bounds grid)) indices
      word = map (grid A.!) indices
   in inRange && word == "XMAS"

wordIndices :: Pos -> [[Pos]]
wordIndices start =
  let one :: Pos -> Pos -> [Pos]
      one (row, col) (rowDiff, colDiff) =
        [(row + (rowDiff * x), col + (colDiff * x)) | x <- [0 .. 3]]
      adjacent = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1], (r, c) /= (0, 0)]
   in map (one start) adjacent

part2 :: String -> Int
part2 = dfs2 . parse

dfs2 :: Grid -> Int
dfs2 grid =
  let crosses = map masIndices $ A.indices grid
   in length $ filter (isCrossMas grid) crosses

isCrossMas :: Grid -> [[Pos]] -> Bool
isCrossMas grid words =
  let inRange = all (A.inRange (A.bounds grid)) $ join words
      isMas = (== "MAS") . map (grid A.!)
      mases = filter isMas words
   in inRange && length mases == 2

masIndices :: Pos -> [[Pos]]
masIndices (r, c) =
  let one :: Pos -> Pos -> [Pos]
      one (row, col) (rowDiff, colDiff) =
        [(row + (rowDiff * x), col + (colDiff * x)) | x <- [0 .. 2]]
   in [ one (r - 1, c - 1) (1, 1),
        one (r - 1, c + 1) (1, -1),
        one (r + 1, c + 1) (-1, -1),
        one (r + 1, c - 1) (-1, 1)
      ]
