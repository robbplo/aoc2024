module Day12
  ( part1,
    part2,
  )
where

import Data.Function
import Debug.Trace

-- North/South, East/West, Rotation degrees starting East
type Pos = (Int, Int, Int)

part1 :: String -> Int
part1 input = abs north + abs east
  where
    instructions = parse input
    (north, east, _) = foldr move (0, 0, 0) instructions

parse :: String -> [(Char, Int)]
parse = map parseLine . lines

parseLine :: String -> (Char, Int)
parseLine (x : xs) = (x, read xs)

move :: (Char, Int) -> Pos -> Pos
move ('N', x) (n, e, r) = (n + x, e, r)
move ('S', x) (n, e, r) = (n - x, e, r)
move ('E', x) (n, e, r) = (n, e + x, r)
move ('W', x) (n, e, r) = (n, e - x, r)
move ('L', x) (n, e, r) = (n, e, degrees (r - x))
move ('R', x) (n, e, r) = (n, e, degrees (r + x))
move ('F', x) (n, e, r) = case r of
  0 -> move ('E', x) (n, e, r)
  90 -> move ('S', x) (n, e, r)
  180 -> move ('W', x) (n, e, r)
  270 -> move ('N', x) (n, e, r)
  num -> error $ show num

degrees n
  | n >= 360 = degrees (n - 360)
  | n < 0 = degrees (n + 360)
  | otherwise = n

part2 :: String -> Int
part2 input = abs north + abs east
  where
    start = (1, 10, 0)
    instructions = parse input
    (north, east, _) = go instructions (0, 0, 0) start

go :: [(Char, Int)] -> Pos -> Pos -> Pos
go [] pos waypoint = pos
go (('F', x) : xs) pos waypoint = go xs (add pos waypoint x) waypoint
go (x : xs) pos waypoint = go xs pos (moveWaypoint x waypoint)

add (x1, y1, _) (x2, y2, r) times = (x1 + (x2 * times), y1 + (y2 * times), r)

moveWaypoint :: (Char, Int) -> Pos -> Pos
moveWaypoint ('N', x) (n, e, r) = (n + x, e, r)
moveWaypoint ('S', x) (n, e, r) = (n - x, e, r)
moveWaypoint ('E', x) (n, e, r) = (n, e + x, r)
moveWaypoint ('W', x) (n, e, r) = (n, e - x, r)
moveWaypoint ('L', x) (n, e, r) = rotate (n, e, r) (matrix (-x))
moveWaypoint ('R', x) (n, e, r) = rotate (n, e, r) (matrix x)
moveWaypoint ('F', x) pos = pos

matrix :: Int -> (Int, Int, Int, Int)
matrix angle =
  ( round $ cos t,
    round $ -sin t,
    round $ sin t,
    round $ cos t
  )
  where
    t = fromIntegral angle * (pi / 180)

rotate :: Pos -> (Int, Int, Int, Int) -> Pos
rotate (x, y, r) (x1, x2, y1, y2) = (x', y', 0)
  where
    x' = (x * x1) + (y * x2)
    y' = (x * y1) + (y * y2)
