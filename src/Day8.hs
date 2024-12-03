module Day8
  ( part1,
    part2,
  )
where

import Control.Monad
import Data.Array
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

part1 :: String -> Int
part1 input = findCycle array S.empty (0, 0)
  where
    array = parse input

findCycle :: Array Int (String, Int) -> S.Set Int -> (Int, Int) -> Int
findCycle array seen (acc, pos)
  | pos `S.member` seen = acc
  | otherwise = findCycle array (S.insert pos seen) (step array (acc, pos))

parse :: String -> Array Int (String, Int)
parse input = listArray (0, length l) (map splitLine l)
  where
    l = lines input
    toInt ('+' : str) = read str :: Int
    toInt str = read str :: Int
    splitLine str = (take 3 str, toInt $ drop 4 str)

step :: Array Int (String, Int) -> (Int, Int) -> (Int, Int)
step array (acc, pos) = step' array (acc, pos) (array ! pos)

step' :: Array Int (String, Int) -> (Int, Int) -> (String, Int) -> (Int, Int)
step' array (acc, pos) ("nop", num) = (acc, pos + 1)
step' array (acc, pos) ("acc", num) = (acc + num, pos + 1)
step' array (acc, pos) ("jmp", num) = (acc, pos + num)

part2 :: String -> Int
part2 input = fst . fromJust $ findEnd array S.empty False (0, 0)
  where
    array = parse input

findEnd :: Array Int (String, Int) -> S.Set Int -> Bool -> (Int, Int) -> Maybe (Int, Int)
findEnd array seen changed (acc, pos)
  | pos `S.member` seen = Nothing
  | pos >= length array - 1 = Just (acc, pos)
  | traceShow (acc, pos, instruction, num) False = Nothing
  | not changed && instruction == "nop" = next False (doStep ("nop", num)) `mplus` next True (doStep ("jmp", num))
  | not changed && instruction == "jmp" = next False (doStep ("jmp", num)) `mplus` next True (doStep ("nop", num))
  | otherwise = next changed (step array (acc, pos))
  where
    (instruction, num) = array ! pos
    next = findEnd array (S.insert pos seen)
    canSwitch = not changed && instruction `elem` ["nop", "jmp"]
    doStep = step' array (acc, pos)
