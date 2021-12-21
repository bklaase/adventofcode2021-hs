module Solutions.Day4Spec where

import Test.Hspec
import Solutions.Day4

test_input :: String
test_input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

spec = context "Day 4" $ do
  it "Should parse test input as Game" $ do
    take 2 (fst (parse test_input)) `shouldBe` [7,4]
    take 2 (head (head (snd (parse test_input)))) `shouldBe` [22,13]

  -- it "should calculate test output for part 1" $
  --   part1 (parse test_input) `shouldBe` 198

  -- it "should apply bitmask filter for a position correctly" $
  --   length (filterByMaskPos 0 (readBitSeq "1" ) (parse test_input)) `shouldBe` 7

  -- it "should calculate test output for part 2" $
  --   part2 (parse test_input) `shouldBe` 220
