module Day5Spec (spec) where

import Day5
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "BFFFBBFRRR",
        "FFFBBBFRRR",
        "BBFFBBFRLL"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 820
