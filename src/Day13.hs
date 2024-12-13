module Day13
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import Debug.Trace

type Pos = (Int, Int) -- X, Y

type Machine = (Pos, Pos, Pos) -- A, B, Prize

type State = (Int, Int) -- As, Bs

parse :: String -> [Machine]
parse input = map parseMachine $ splitOn "\n\n" input

parseMachine :: String -> Machine
parseMachine input =
  let l = lines input
      button = map (drop 2) . splitOn ", " . last . splitOn ": "
      pos x = let b = button x in (read (head b), read (last b))
   in (pos (head l), pos (l !! 1), pos (l !! 2))

part1 :: String -> Int
part1 input = sum . traceShowId . map (minTokens (0, 0) 0) $ parse input

minTokens :: State -> Int -> Machine -> Int
minTokens (as, bs) min (a, b, goal)
  | as > 100 = min
  | pos == goal = traceShow (as, bs) next (as + 1, bs - 3) ((as * 3) + bs)
  | fst pos > fst goal || snd pos > snd goal = next (as + 1, bs - 3) min
  | otherwise = next (as, bs + 1) min
  where
    pos = curr (a, b, goal) (as, bs)
    next state' min' = minTokens state' min' (a, b, goal)

curr :: Machine -> State -> Pos
curr ((ax, ay), (bx, by), _) (as, bs) = (ax * as + bx * bs, ay * as + by * bs)

part2 :: String -> Int
part2 input = undefined
