module Solutions.Day2Spec where

import Test.Hspec
import Solutions.Day2

test_input :: String
test_input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

spec = context "Day 2" $ do
  it "Should parse test input as [Direction]" $
    parse test_input `shouldBe` [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

  it "should calculate test output for part 1" $
    part1 (parse test_input) `shouldBe` 150

  it "should calculate test output for part 2" $
    part2 (parse test_input) `shouldBe` 900
