module Day5
  ( part1,
    part2,
  )
where

import Data.List (groupBy, sort, sortBy, (\\))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Debug.Trace
import GHC.Float

type Rule = (Int, Int)

type Update = [Int]

type RuleSet = M.Map Int [Int]

part1 :: String -> Int
part1 input =
  let (rules, updates) = parse input
      validUpdates = filter (\u -> checkUpdate rules u []) updates
   in sum . map middle $ validUpdates

checkUpdate :: [Rule] -> Update -> [Int] -> Bool
checkUpdate rules [] seen = True
checkUpdate rules (x : update) seen
  | all (\r -> checkRule r x seen) rules = checkUpdate rules update (x : seen)
  | otherwise = False

checkRule :: Rule -> Int -> [Int] -> Bool
checkRule (a, b) num seen
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
part2 input =
  let (rules, updates) = parse input
      invalidUpdates = filter (\u -> not $ checkUpdate rules u []) updates
      ruleset = combineRules rules
      fixedUpdates = map (sortBy (compareRule ruleset)) invalidUpdates
   in sum . map middle $ fixedUpdates

combineRules :: [Rule] -> RuleSet
combineRules rules =
  let groups = groupBy (\(a, b) (c, d) -> a == c) $ sort rules
      combine = foldr (\(y, x) (_, xs) -> (y, x : xs)) (0, [])
   in M.fromList $ map combine groups

compareRule :: RuleSet -> Int -> Int -> Ordering
compareRule ruleset a b = case ruleset M.!? a of
  Just after -> if b `elem` after then LT else GT
  Nothing -> GT
