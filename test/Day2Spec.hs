module Day2Spec (spec) where

import Day2
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "7 6 4 2 1",
        "1 2 7 8 9",
        "9 7 6 2 1",
        "1 3 2 4 5",
        "8 6 4 4 1",
        "1 3 6 7 9"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 2
  it "part 2" $ do
    part2 input `shouldBe` 4
