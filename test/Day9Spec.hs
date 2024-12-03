module Day9Spec (spec) where

import Day9
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "35",
        "20",
        "15",
        "25",
        "47",
        "40",
        "62",
        "55",
        "65",
        "95",
        "102",
        "117",
        "150",
        "182",
        "127",
        "219",
        "299",
        "277",
        "309",
        "576"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1' 5 input `shouldBe` 127
  it "part 2" $ do
    part2' 5 input `shouldBe` 62
