{-# LANGUAGE TupleSections #-}

module Day17
  ( part1,
    part2,
  )
where

import Control.Monad
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace

type Pos = (Int, Int)

type Grid = Map Pos Int

part1 :: String -> Int
part1 input = error "not implemented"

part1' :: String -> Pos -> Int
part1' input exit =
  let grid = parse input
   in error "not implemented"

parse :: String -> Grid
parse str = Map.fromList $ ((0, 0), 0) : pairs
  where
    pairs = map ((,-1) . line) (take 12 $ lines str)

bfs :: Grid -> [Pos]
line :: String -> Pos
line l = read $ "(" <> l <> ")"

part2 :: String -> Int
part2 input = error "not implemented"
