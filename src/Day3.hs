module Day3
  ( part1,
    part2,
  )
where

import qualified Data.Vector as V
import Debug.Trace (traceShowId)

part1 :: String -> Int
part1 input = countSlope rows 1 3
  where
    rows = lines input
    width = length $ head rows

enumerate = zip [0 ..]

part2 :: String -> Int
part2 input =
  product
    [ countSlope rows 1 1,
      countSlope rows 1 3,
      countSlope rows 1 5,
      countSlope rows 1 7,
      countSlope rows 2 1
    ]
  where
    rows = lines input
    width = length $ head rows

countSlope rows down right =
  length $
    [ char
      | (row_ix, row) <- enumerate rows,
        row_ix `mod` down == 0,
        (char_ix, char) <- enumerate row,
        char_ix == ((row_ix `div` down) * right) `mod` width,
        char == '#'
    ]
  where
    width = length $ head rows
