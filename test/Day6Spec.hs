module Day6Spec (spec) where

import Day6
import Test.Hspec

input :: String
input =
  init $
    unlines
      [ "....#.....",
        ".........#",
        "..........",
        "..#.......",
        ".......#..",
        "..........",
        ".#..^.....",
        "........#.",
        "#.........",
        "......#..."
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 41
  it "part 2" $ do
    part2 input `shouldBe` 6
