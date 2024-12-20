module Day6
  ( part1,
    part2,
  )
where

import Control.Monad (join)
import qualified Data.Array.IArray as A
import Data.Bifunctor (second)
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V
import Debug.Trace

type Pos = (Int, Int)

type Grid = V.Vector (V.Vector Char)

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
      grid = V.fromList $ map V.fromList l
      guardPos = gridFind grid '^'
   in (grid, (guardPos, (-1, 0)))

step :: Grid -> Guard -> Maybe Guard
step grid (pos, dir) =
  let newPos = addPos pos dir
   in case grid A.!? newPos of
        Nothing -> Nothing
        Just '#' -> Just (pos, turnRight dir)
        _ -> Just (newPos, dir)

gridFind :: Grid -> Char -> Pos
gridFind grid char = head $ V.mapMaybe inRow grid
  where
    inRow row = V.elemIndex char row

addPos :: Pos -> Pos -> Pos
addPos (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

turnRight :: Pos -> Pos
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)

part2 :: String -> Int
part2 input =
  let (grid, guard) = parse input
   in length $ filter (\o -> loops grid guard o S.empty) (A.indices grid)

loops :: Grid -> Guard -> Pos -> S.Set Guard -> Bool
loops grid guard override seen
  | guard `S.member` seen = True
  | otherwise =
      let newPos = uncurry addPos guard
       in case grid A.!? newPos of
            Nothing -> False
            Just '#' -> turn
            _ -> if newPos == override then turn else step newPos
  where
    turn = loops grid (second turnRight guard) override (S.insert guard seen)
    step newPos = loops grid (newPos, snd guard) override (S.insert guard seen)
