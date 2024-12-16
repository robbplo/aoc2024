module Day16
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.List (find)
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

part1 :: String -> Int
part1 input = search . parse $ input

parse :: String -> Grid
parse str =
  let l = lines str
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

start :: Grid -> Pos
start grid = fst . fromJust . find ((== 'S') . snd) $ A.assocs grid

search :: Grid -> Int
search grid = fromJust $ search' grid (start grid) (0, 1) S.empty

search' :: Grid -> Pos -> Pos -> S.Set Pos -> Maybe Int
search' grid pos dir visit = case grid A.! pos of
  'E' -> Just 0
  '#' -> Nothing
  _ ->
    if pos `S.member` visit
      then Nothing
      else case catMaybes
        [ (+) 1 <$> forward,
          (+) 1001 <$> right,
          (+) 1001 <$> left
        ] of
        [] -> Nothing
        results -> Just (minimum results)
    where
      visit' = S.insert pos visit
      forward = search' grid (addPos pos dir) dir visit'
      right = search' grid (addPos pos (turnRight dir)) (turnRight dir) visit'
      left = search' grid (addPos pos (turnLeft dir)) (turnLeft dir) visit'

turnRight :: Pos -> Pos
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)

turnLeft :: Pos -> Pos
turnLeft (-1, 0) = (0, -1)
turnLeft (0, 1) = (-1, 0)
turnLeft (1, 0) = (0, 1)
turnLeft (0, -1) = (1, 0)

addPos :: Pos -> Pos -> Pos
addPos (a, b) (c, d) = (a + c, b + d)

part2 :: String -> Int
part2 input = error "not implemented"
