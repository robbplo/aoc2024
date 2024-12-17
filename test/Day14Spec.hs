module Day14Spec (spec) where

import Day14
import Test.Hspec

input :: String
input =
  init . unlines $
    [ "p=0,4 v=3,-3",
      "p=6,3 v=-1,-3",
      "p=10,3 v=-1,2",
      "p=2,0 v=2,-1",
      "p=0,0 v=1,3",
      "p=3,0 v=-2,-2",
      "p=7,6 v=-1,-3",
      "p=3,0 v=-1,-2",
      "p=9,3 v=2,3",
      "p=7,3 v=-1,2",
      "p=2,4 v=2,-3",
      "p=9,5 v=-3,-3"
    ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1' input 11 7 `shouldBe` 12
  it "part 2" $ do
    part2 input `shouldBe` 62
