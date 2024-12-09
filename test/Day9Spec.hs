module Day9Spec (spec) where

import Day9
import Test.Hspec

input :: String
input = "2333133121414131402"

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 1928
  it "part 2" $ do
    part2 input `shouldBe` 62
