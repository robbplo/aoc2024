module Day8
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.Bifunctor
import Data.List (groupBy, nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

type Antenna = (Pos, Char)

part1 :: String -> Int
part1 input =
  let grid = parse input
   in length . nub . join . map (antinodes grid) . antennas $ grid

parse :: String -> Grid
parse input =
  let l = lines input
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

antennas :: Grid -> [(Char, [Pos])]
antennas grid =
  let assocs = sortBy (\a b -> compare (snd a) (snd b)) . filter ((/= '.') . snd) $ A.assocs grid
      groups = groupBy (\a b -> snd a == snd b) assocs
      combine = foldr (\(pos, char) (_, acc) -> (char, pos : acc)) ('0', [])
   in traceShowId $ map combine groups

antinodes :: Grid -> (Char, [Pos]) -> [Pos]
antinodes grid (char, positions) =
  let inBounds = A.inRange $ A.bounds grid
      pairs = [(a, b) | a <- positions, b <- positions, a /= b]
      anti (a, b) = addPos a (subPos a b)
   in filter inBounds $ map anti pairs

allAntinodes :: Grid -> (Char, [Pos]) -> [Pos]
allAntinodes grid (char, positions) =
  let inBounds = A.inRange $ A.bounds grid
      pairs = [(a, b) | a <- positions, b <- positions, a /= b]
      nodes (a, b) = takeWhile inBounds $ inLine a b
   in join $ map nodes pairs

inLine :: Pos -> Pos -> [Pos]
inLine a b =
  let next = flip addPos (subPos a b)
   in iterate next a

addPos :: Pos -> Pos -> Pos
addPos (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

subPos :: Pos -> Pos -> Pos
subPos (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

part2 :: String -> Int
part2 input =
  let grid = parse input
   in length . nub . join . map (allAntinodes grid) . antennas $ grid
