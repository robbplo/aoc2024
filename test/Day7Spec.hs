module Day7Spec (spec) where

import Day7
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "190: 10 19",
        "3267: 81 40 27",
        "83: 17 5",
        "156: 15 6",
        "7290: 6 8 6 15",
        "161011: 16 10 13",
        "192: 17 8 14",
        "21037: 9 7 18 13",
        "292: 11 6 16 20"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 3749
  it "part 2" $ do
    part2 input `shouldBe` 11387
