module Day12Spec (spec) where

import Day12
import Test.Hspec

input :: String
input = init . unlines $ [ 
  "AAAA",
  "BBCD",
  "BBCC",
  "EEEC" ]


input2 :: String
input2 = init . unlines $ [ "RRRRIICCFF",
  "RRRRIICCCF",
  "VVRRRCCFFF",
  "VVRCCCJFFF",
  "VVVVCJJCFE",
  "VVIVCCJJEE",
  "VVIIICJJEE",
  "MIIIIIJJEE",
  "MIIISIJEEE",
  "MMMISSJEEE" ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 140
    part1 input2 `shouldBe` 1930
  it "part 2" $ do
    part2 "1" `shouldBe` 62
