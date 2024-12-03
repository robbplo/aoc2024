module Day8Spec (spec) where

import Day8
import Test.Hspec

input :: String
input =
  init $
    unlines
      [
        "nop +0",
        "acc +1",
        "jmp +4",
        "acc +3",
        "jmp -3",
        "acc -99",
        "acc +1",
        "jmp -4",
        "acc +6"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 5
  it "part 2" $ do
    part2 input `shouldBe` 8
