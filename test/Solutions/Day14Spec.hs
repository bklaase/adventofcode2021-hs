{-# LANGUAGE RecordWildCards #-}
module Solutions.Day14Spec where

import Test.Hspec
import Solutions.Day14

test_input :: String
test_input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
spec = context "Day 14" $ do
  let input = parse test_input
  it "Should correctly parse test input." $ do
    let Rule {..} = (head . rules) input
    template input `shouldBe` "NNCB"
    ins `shouldBe` 'B'
    p1 `shouldBe` 'C'
  
  it "Should correctly apply rules" $ do
    let applied = applyRulesToInput (rules input) (template input)
    applied `shouldBe` "NCNBCHB"

  it "Should correctly apply rules multiple times" $ do
    let applied = applyNTimes (applyRulesToInput (rules input)) 4 (template input)
    applied `shouldBe` "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

  it "should calculate test output for part 1" $
     part1 (parse test_input) `shouldBe` 1588

  -- it "should apply bitmask filter for a position correctly" $
  --   length (filterByMaskPos 0 (readBitSeq "1" ) (parse test_input)) `shouldBe` 7

  -- it "should calculate test output for part 2" $
  --   part2 (parse test_input) `shouldBe` 220
