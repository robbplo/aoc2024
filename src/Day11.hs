module Day11
  ( part1,
    part2,
  )
where

import Control.Monad
import Data.Array as A
import Debug.Trace

type Pos = (Int, Int)

type Grid a = Array Pos a

part1 :: String -> Int
part1 input = length . filter (== '#') . A.elems . part1' $ parse input

x = take 10 $ map ((*) 2 . (+) 1) [1..]

part1' :: Grid Char -> Grid Char
part1' grid
  | next == grid = grid
  | otherwise = part1' next
  where
    next = swap grid

parse :: String -> Grid Char
parse input = A.listArray ((1, 1), (rows, cols)) $ join l
  where
    l = lines input
    rows = length l
    cols = length $ head l

swap :: Grid Char -> Grid Char
swap grid = grid A.// swap' grid (A.indices grid)

swap' :: Grid Char -> [Pos] -> [(Pos, Char)]
swap' grid [] = []
swap' grid (x : xs)
  | grid ! x == 'L' && adj == 0 = (x, '#') : next
  | grid ! x == '#' && adj >= 4 = (x, 'L') : next
  | otherwise = next
  where
    next = swap' grid xs
    adj = length . filter (== '#') $ adjacent grid x

put :: Grid a -> Pos -> a -> Grid a
put grid pos elem = grid A.// [(pos, elem)]

inBounds :: Grid a -> Pos -> Bool
inBounds arr = A.inRange (A.bounds arr)

addPos :: Pos -> Pos -> Pos
addPos (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

adjacent :: Grid a -> Pos -> [a]
adjacent grid pos = map (grid !) positions
  where
    positions = filter (inBounds grid) $ map (addPos pos) directions

directions :: [Pos]
directions =
  [ (row, col)
    | row <- [-1 .. 1],
      col <- [-1 .. 1],
      (row, col) /= (0, 0)
  ]

seatsSeen :: Grid Char -> Pos -> Int
seatsSeen grid pos = length $ filter (seen grid pos) directions

seen :: Grid Char -> Pos -> Pos -> Bool
seen grid pos dir
  | not $ inBounds grid next = False
  | grid ! next == 'L' = False
  | grid ! next == '#' = True
  | otherwise = seen grid next dir
  where next = addPos pos dir

part2 :: String -> Int
part2 input = length . filter (== '#') . A.elems . part2' $ parse input

part2' :: Grid Char -> Grid Char
part2' grid
  | next == grid = grid
  | otherwise = part2' next
  where
    next = swap2 grid

swap2 :: Grid Char -> Grid Char
swap2 grid = grid A.// swap2' grid (A.indices grid)

swap2' :: Grid Char -> [Pos] -> [(Pos, Char)]
swap2' grid [] = []
swap2' grid (x : xs)
  | grid ! x == 'L' && adj == 0 = (x, '#') : next
  | grid ! x == '#' && adj >= 5 = (x, 'L') : next
  | otherwise = next
  where
    next = swap2' grid xs
    adj = seatsSeen grid x
