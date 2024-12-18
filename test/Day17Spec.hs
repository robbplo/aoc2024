module Day17Spec (spec) where

import Day17
import Test.Hspec

input :: String
input =
  init . unlines $
    [ "5,4",
      "4,2",
      "4,5",
      "3,0",
      "2,1",
      "6,3",
      "2,4",
      "1,5",
      "0,6",
      "3,3",
      "2,6",
      "5,1",
      "1,2",
      "5,5",
      "2,5",
      "6,5",
      "1,4",
      "0,4",
      "6,4",
      "1,1",
      "6,1",
      "1,0",
      "0,5",
      "1,6",
      "2,0"
    ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 22
  it "part 2" $ do
    part2 input `shouldBe` 9021
