module Day13
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import Debug.Trace

type Pos = (Int, Int) -- X, Y

type Machine = (Pos, Pos, Pos) -- A, B, Prize

parse :: String -> [Machine]
parse input = map parseMachine $ splitOn "\n\n" input

parseMachine :: String -> Machine
parseMachine input =
  let l = lines input
      button = map (drop 2) . splitOn ", " . last . splitOn ": "
      pos x = let b = button x in (read (head b), read (last b))
   in (pos (head l), pos (l !! 1), pos (l !! 2))

part1 :: String -> Int
part1 input = sum . map dingin $ parse input

dingin :: Machine -> Int
dingin (a, b, prize) = let
    base = (det a b)
    bs = (det a prize) `div` base
    as = (det b prize) `div` base
    isInt = bs * base == (det a prize) && as * base == (det b prize)
  in
  if isInt then (abs(as) * 3) + abs(bs) else 0

det :: Pos -> Pos -> Int
det (a, b) (c, d) = (a * d) - (b * c)

part2 :: String -> Int
part2 input = sum . map (dingin . addLoads) $ parse input

addLoads :: Machine -> Machine
addLoads (a, b, (x, y)) = (a, b, (x + loads, y + loads))
  where loads = 10000000000000
