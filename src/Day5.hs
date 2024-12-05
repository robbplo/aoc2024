module Day5
  ( part1,
    part2,
  )
where

import Data.List ((\\))
import Data.List.Split (splitOn)
import Debug.Trace
import GHC.Float

type Rule = (Int, Int)

type Update = [Int]

part1 :: String -> Int
part1 input =
  let (rules, updates) = traceShowId $ parse input
      validUpdates = traceShowId $ filter (\u -> checkUpdate rules u []) updates
   in sum . map middle $ validUpdates

checkUpdate :: [Rule] -> Update -> [Int] -> Bool
checkUpdate rules [] seen = True
checkUpdate rules (x : update) seen
  | all (\r -> checkRule r x seen) rules = checkUpdate rules update (x : seen)
  | otherwise = False

checkRule :: Rule -> Int -> [Int] -> Bool
checkRule (a, b) num seen
  -- \| traceShow [a, b, num] False = undefined
  | num == a = b `notElem` seen
  | otherwise = True

middle :: Update -> Int
middle update = update !! (length update `div` 2)

parse :: String -> ([Rule], [Update])
parse input =
  let parts = splitOn "\n\n" input
      rules = lines . head $ parts
      updates = lines . last $ parts
   in (map parseRule rules, map parseUpdate updates)

parseRule :: String -> Rule
parseRule input =
  let parts = splitOn "|" input
   in (read . head $ parts, read . last $ parts)

parseUpdate :: String -> Update
parseUpdate input =
  let parts = splitOn "," input
   in map read parts

part2 :: String -> Int
part2 input = undefined
