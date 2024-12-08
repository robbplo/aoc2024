module Day8Spec (spec) where

import Day8
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "............",
        "........0...",
        ".....0......",
        ".......0....",
        "....0.......",
        "......A.....",
        "............",
        "............",
        "........A...",
        ".........A..",
        "............",
        "............"
      ]

input2 =
  init $
    unlines
      [ "..........",
        "..........",
        "..........",
        "....a.....",
        "........a.",
        ".....a....",
        "..........",
        "......A...",
        "..........",
        ".........."
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input2 `shouldBe` 4
    part1 input `shouldBe` 14
  it "part 2" $ do
    part2 input `shouldBe` 34
