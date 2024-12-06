module Day6
  ( part1,
    part2,
  )
where

import Control.Monad (join)
import qualified Data.Array.IArray as A
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

type Guard = (Pos, Pos) -- Position, Direction

part1 :: String -> Int
part1 input = (+ 1) $ length . nub $ uncurry patrol $ parse input

patrol :: Grid -> Guard -> [Pos]
patrol grid (pos, dir) = case step grid (pos, dir) of
  Nothing -> []
  Just guard -> pos : patrol grid guard

parse :: String -> (Grid, Guard)
parse input =
  let l = lines input
      rows = length l
      cols = length $ head l
      grid = A.listArray ((1, 1), (rows, cols)) $ join l
      guardPos = fst $ fromJust $ find ((== '^') . snd) (A.assocs grid)
   in (grid, (guardPos, (-1, 0)))

step :: Grid -> Guard -> Maybe Guard
step grid (pos, dir) =
  let newPos = traceShow (pos, dir) $ addPos pos dir
   in case grid A.!? newPos of
        Nothing -> Nothing
        Just '#' -> Just (pos, turnRight dir)
        _ -> Just (newPos, dir)

addPos :: Pos -> Pos -> Pos
addPos (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

turnRight :: Pos -> Pos
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)

part2 :: String -> Int
part2 input = undefined
