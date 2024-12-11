module Day11Spec (spec) where

import Day11
import Test.Hspec

input :: String
input = "125 17"

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 55312
  it "part 2" $ do
    part2 "1" `shouldBe` 62
