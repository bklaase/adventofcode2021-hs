module Solutions.Day2Spec where

import Test.Hspec
import Solutions.Day3

test_input :: String
test_input = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"
spec = context "Day 3" $ do
  it "Should parse test input as [Bits]" $
    take 3 (head $ parse test_input) `shouldBe` [False, False, True]

  it "should calculate test output for part 1" $
    part1 (parse test_input) `shouldBe` 198

  it "should apply bitmask filter for a position correctly" $
    length (filterByMaskPos 0 (readBitSeq "1" ) (parse test_input)) `shouldBe` 7

  it "should calculate test output for part 2" $
    part2 (parse test_input) `shouldBe` 230
