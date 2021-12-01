module Solutions.Day1 where

import qualified AoC.Lib as L

day1 = L.Solution 1 parse part1 part2

parse :: (Integral a, Read  a) => String -> [a]
parse = map read . lines

part1 :: (Integral a) => [a] -> a
part1 = countIncreases

part2 :: (Integral a) => [a] -> a
part2 xs = countIncreases xs3
  where xs2 = zipWith (+) xs (tail xs)
        xs3 = zipWith (+) xs2 (tail (tail xs))

countIncreases :: (Integral a) => [a] -> a
countIncreases (x:y:xs)
  | x < y = 1 + countIncreases (y:xs)
  | otherwise = countIncreases (y:xs)
countIncreases _ = 0
