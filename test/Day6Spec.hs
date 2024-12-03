module Day6Spec (spec) where

import Day6
import Test.Hspec

input :: String
input =
  init $
    unlines
      [
        "abc",
        "",
        "a",
        "b",
        "c",
        "",
        "ab",
        "ac",
        "",
        "a",
        "a",
        "a",
        "a",
        "",
        "b"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 11
  it "part 2" $ do
    part2 input `shouldBe` 6
