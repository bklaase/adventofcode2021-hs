module Solutions.Day1Spec where

import Test.Hspec
import Solutions.Day1

test_input_day1_1 :: [Int]
test_input_day1_1 = [199,200,208,210,200,207,240,269,260,263]

spec = context "part1" $ do
  it "should calculate output for part 1" $
    part1 test_input_day1_1 `shouldBe` 7

  it "should calculate output for part 2" $
    part2 test_input_day1_1 `shouldBe` 5
