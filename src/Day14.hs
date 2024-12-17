module Day14
  ( part1,
    part1',
    part2,
    part2',
  )
where

import Data.List (group, sort)
import Data.List.Split
import Debug.Trace

type Pos = (Int, Int)

type Robot = (Pos, Pos)

parse :: String -> [Robot]
parse input = map line $ lines input
  where
    line str =
      let parts = words str
       in (pos (head parts), pos (last parts))
    pos str =
      let parts = splitOn "," (drop 2 str)
       in (read (head parts), read (last parts)) :: (Int, Int)

move :: Int -> Robot -> Pos
move seconds (pos, velocity) = addPos pos $ multPos velocity seconds

part1 :: String -> Int
part1 input = part1' input 101 103

part1' :: String -> Int -> Int -> Int
part1' input width height =
  let grid = (width, height)
      quadrants = filter (> 0) . map ((`inQuadrant` grid) . (`modPos` grid) . move 100) $ parse input
      groups = map length . group . sort $ quadrants
   in product groups

part2 :: String -> Int
part2 input = error "not implemented"

part2' :: String -> Int -> Int -> Int
part2' input = error "not implemented"

addPos :: Pos -> Pos -> Pos
addPos (a, b) (c, d) = (a + c, b + d)

multPos :: Pos -> Int -> Pos
multPos (a, b) c = (a * c, b * c)

modPos :: Pos -> Pos -> Pos
modPos (a, b) (c, d) = traceShow (a, b, c, d) (a `mod` c, b `mod` d)

inQuadrant :: Pos -> Pos -> Int
inQuadrant (a, b) (c, d)
  | a < midX && b < midY = 1
  | a > midX && b < midY = 2
  | a < midX && b > midY = 3
  | a > midX && b > midY = 4
  | otherwise = 0
  where
    midX = (c `div` 2) + 1
    midY = (d `div` 2) + 1
