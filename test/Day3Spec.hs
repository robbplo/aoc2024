module Day3Spec (spec) where

import Day3
import Test.Hspec

input :: String
input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 161
  it "part 2" $ do
    part2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" `shouldBe` 48
