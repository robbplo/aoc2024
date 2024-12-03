{-# LANGUAGE OverloadedStrings #-}

module Day6
  ( part1,
    part2,
  )
where

import qualified Data.Text as T
import Data.List (nub, intersect)

part1 :: String -> Int
part1 input = sum counts
  where
    split = map (T.unpack . T.replace "\n" "") (T.splitOn "\n\n" $ T.pack input)
    counts = map (length . nub) split


part2 :: String -> Int
part2 input = sum counts
  where
    split = map (lines . T.unpack) (T.splitOn "\n\n" $ T.pack input)
    counts = map (length . intersectAll) split

intersectAll :: [String] -> String
intersectAll [] = ['a'..'z']
intersectAll (x:xs) = intersect x $ intersectAll xs



