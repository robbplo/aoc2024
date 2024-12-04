module Day4Spec (spec) where

import Day4
import Test.Hspec

input :: String
input =
  init $
    unlines
      [
  "MMMSXXMASM",
  "MSAMXMSMSA",
  "AMXSXMAAMM",
  "MSAMASMSMX",
  "XMASAMXAMM",
  "XXAMMXXAMA",
  "SMSMSASXSS",
  "SAXAMASAAA",
  "MAMMMXMMMM",
  "MXMXAXMASX"
      ]

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 18
  it "part 2" $ do
    part2 input `shouldBe` 9
