module Day10Spec (spec) where

import Day10
import Test.Hspec

input :: String
input =
  init . unlines $
    [ "89010123",
      "78121874",
      "87430965",
      "96549874",
      "45678903",
      "32019012",
      "01329801",
      "10456732"
    ]

smallinput =
  init . unlines $
    [ "8880888",
      "8881888",
      "8882888",
      "6543456",
      "7888887",
      "8888888",
      "9888889"
    ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 smallinput `shouldBe` 2
    part1 input `shouldBe` 36
  it "part 2" $ do
    part2 input `shouldBe` 62
