module Day3Spec (spec) where

import Day3
import Test.Hspec

input :: String
input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

spec :: Spec
spec = do
  it "part 1" $ do
    part1 input `shouldBe` 7
  it "part 2" $ do
    part2 input `shouldBe` 336
