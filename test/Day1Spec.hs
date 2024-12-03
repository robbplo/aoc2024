module Day1Spec (spec) where

import Day1
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "3   4",
        "4   3",
        "2   5",
        "1   3",
        "3   9",
        "3   3"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 11
  it "part 2" $ do
    part2 input `shouldBe` 31
