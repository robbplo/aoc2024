module Day7
  ( part1,
    part2,
  )
where

import Data.List.Split

type Equation = (Int, [Int])

part1 :: String -> Int
part1 input = sum . map fst . filter (`isValid` 0) $ parse input

parse :: String -> [Equation]
parse input = map line $ lines input

line :: String -> Equation
line str =
  let parts = splitOn ": " str
      nums = words . last $ parts
   in (read . head $ parts, map read nums)

isValid :: Equation -> Int -> Bool
isValid (sum, []) total = total == sum
isValid (sum, x : xs) total = next (x + total) || next (x * total)
  where
    next = isValid (sum, xs)

part2 :: String -> Int
part2 input = sum . map fst . filter (`isValidConcat` 0) $ parse input

isValidConcat :: Equation -> Int -> Bool
isValidConcat (sum, []) total = total == sum
isValidConcat (sum, x : xs) total = next (x + total) || next (x * total) || next (concatInt total x)
  where
    next = isValidConcat (sum, xs)

concatInt :: Int -> Int -> Int
concatInt a b = read $ show a <> show b
