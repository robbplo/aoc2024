module Day15
  ( part1,
    part2,
  )
where

import Control.Monad
import qualified Data.Array.IArray as A
import Data.List (find)
import Data.List.Split
import Data.Maybe
import Debug.Trace

type Pos = (Int, Int)

type Grid = A.Array Pos Char

type Box = (Pos, Pos)

parse :: String -> (Grid, [Char])
parse input =
  let parts = splitOn "\n\n" input
   in (grid (head parts), join . lines . last $ parts)

grid :: String -> Grid
grid str =
  let l = lines str
      rows = length l
      cols = length $ head l
   in A.listArray ((1, 1), (rows, cols)) $ join l

bigParse :: String -> (Grid, [Char])
bigParse input =
  let parts = splitOn "\n\n" input
   in (grid . join . map (grow) $ (head parts), join . lines . last $ parts)
  where
    grow '.' = ".."
    grow '#' = "##"
    grow 'O' = "[]"
    grow '@' = "@."

move :: Char -> (Grid, Pos) -> (Grid, Pos)
move char (grid, pos) =
  let stepPoses = tail . iterate (addPos (dir char)) $ pos
      stepChars = map (grid A.!) stepPoses
      steps = zip stepPoses stepChars
      newPos = traceShow (pos, char) $ fst . head $ steps
   in case grid A.! newPos of
        '#' -> (grid, pos)
        '.' -> (grid A.// [(pos, '.'), (newPos, '@')], newPos)
        'O' ->
          let nextNonBox = find ((/= 'O') . snd) steps
           in case nextNonBox of
                Just (endPos, '.') -> (grid A.// [(endPos, 'O'), (newPos, '@'), (pos, '.')], newPos)
                Just (endPos, '#') -> (grid, pos)
                Just _ -> undefined
                Nothing -> undefined

dir :: Char -> Pos
dir '^' = (-1, 0)
dir 'v' = (1, 0)
dir '>' = (0, 1)
dir '<' = (0, -1)

addPos :: Pos -> Pos -> Pos
addPos (a, b) (c, d) = (a + c, b + d)

score :: Char -> Grid -> Int
score target grid = sum . map (\((r, c), _) -> ((r - 1) * 100) + (c - 1)) . filter ((== target) . snd) $ A.assocs grid

player :: Grid -> Pos
player grid = fst . fromJust . find ((== '@') . snd) $ A.assocs grid

part1 :: String -> Int
part1 input =
  let (grid, chars) = traceShowWith snd $ parse input
      start = player grid
   in score 'O' . fst . foldl (flip move) (grid, start) $ chars

boxesMoving :: Grid -> Box -> Pos -> Maybe [Box]
boxesMoving grid box (0, r) =

toBox :: Pos -> Box
toBox 

part2 :: String -> Int
part2 input = error "not implemented"
