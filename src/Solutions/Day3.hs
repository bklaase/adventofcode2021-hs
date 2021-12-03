module Solutions.Day3 where

import qualified AoC.Lib as L
import qualified Data.Char as C
import Data.Foldable (foldl')

day3 = L.Solution 3 parse part1 part2

type Bits = [Bool]

parse :: String -> [Bits]
parse = map readBitSeq . lines

part1 :: [Bits] -> Int
part1 xs = fromBits gammaBits * fromBits epsilonBits
  where
    (sums, count) = sumAndCountBits xs
    epsilonBits = map (< (count `div` 2)) sums
    gammaBits = map (> (count `div` 2)) sums

part2 :: [Bits] -> Int
part2 xs = fromBits oxygenFiltered * fromBits co2ScrubberFiltered
  where
    (sums, count) = sumAndCountBits xs
    oxygenBits = map (>= (count `div` 2)) sums -- greater or equal means, '1' most frequent, or same nr
    co2ScrubberBits = map (< (count `div` 2)) sums -- strictly lesser means '0' most frequent, or same nr
    oxygenFiltered = filterByBitMask oxygenBits xs
    co2ScrubberFiltered = filterByBitMask co2ScrubberBits xs

sumAndCountBits :: [Bits] -> ([Int],Int)
sumAndCountBits xs = foldl' add (replicate n 0,0) xs
  where n = length $ head xs
        add (sums, count) x = (zipWith (+) sums (map fromEnum x), count+1)

readBitSeq :: String -> [Bool]
readBitSeq = map toBit
  where toBit '0' = False
        toBit '1' = True
        toBit _ = error "Not a valid bit representation."

fromBits :: Bits -> Int
fromBits bs = sum $ zipWith (*)
  (reverse (map fromEnum bs)) [2^n | n <- [0..]]

filterByBitMask :: Bits -> [Bits] -> Bits
filterByBitMask mask bss = tryPos 0 bss
  where
    tryPos n bss = result
      where
        filtered = filterByMaskPos n mask bss
        result = if (length filtered) == 1 then head filtered else tryPos (n+1) filtered

filterByMaskPos :: Int -> Bits -> [Bits] -> [Bits]
filterByMaskPos pos mask = filter ((== (mask !! pos)) . (!! pos))
