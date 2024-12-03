module Day7
  ( part1,
    part2,
    parseLine,
  )
where

import Control.Monad
import Data.Function
import qualified Data.HashMap.Strict as M
import Data.List (nub)
import Data.List.Split

type Hmap = M.HashMap String [(String, Int)]

part1 :: String -> Int
part1 input = length $ nub $ concatMap (\key -> dfs key hm []) (M.keys hm)
  where
    hm = parse input

parse :: String -> Hmap
parse input = M.fromList $ map parseLine $ lines input

parseLine :: String -> (String, [(String, Int)])
parseLine line = (key, map parseValue valueChunks)
  where
    kv = line & init & splitOn " contain "
    key = kv & head & words & take 2 & unwords
    valueChunks = splitOn ", " $ last kv

parseValue :: String -> (String, Int)
parseValue ('n' : 'o' : _) = ("Nothing", 0)
parseValue value = (key, count)
  where
    w = words value
    key = unwords . init . drop 1 $ w
    count = read (head w) :: Int

dfs :: String -> Hmap -> [String] -> [String]
dfs "Nothing" _ _ = []
dfs "shiny gold" _ acc = acc
dfs key hmap acc = concatMap (\next -> dfs (fst next) hmap (key : acc)) bags
  where
    bags = hmap M.! key

part2 :: String -> Int
part2 input = dfs2 hm ("shiny gold", 1) - 1
  where
    hm = parse input

dfs2 :: Hmap -> (String, Int) -> Int
dfs2 _ ("Nothing", _) = 0
dfs2 hmap (key, count) = (count * sum (map (dfs2 hmap) bags)) + count
  where
    bags = hmap M.! key
    next (nextKey, count) = dfs2 hmap nextKey
