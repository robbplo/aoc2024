module Day13
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)

part1 :: String -> Int
part1 input = minWait (parse input) (0, 999999999)
    

parse :: String -> (Int, [Int])
parse input = (start, ids)
  where
    l = lines input
    start = read $ head l
    ids = map read . filter (/= "x") . splitOn "," $ last l

minWait :: (Int, [Int]) -> (Int, Int) -> Int
minWait (_, []) (id, min) = id * min
minWait (start, x:xs) (id, min)
  | wait < min = minWait (start, xs) (x, wait)
  | otherwise = minWait (start, xs) (id, min)
  where 
    wait = abs $ (start `mod` x) - x


part2 :: String -> Int
part2 input = error "not implemented"

parse2 :: String -> [Int]
parse2 input = ids
  where
    line = last $ lines input
    toInt "x" = 0
    toInt c = read c
    ids = map toInt . splitOn "," $ line
