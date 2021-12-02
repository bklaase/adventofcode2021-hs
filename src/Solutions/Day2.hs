module Solutions.Day2 where

import qualified AoC.Lib as L
import qualified Data.Char as C


data Direction = Up Int | Down Int | Forward Int
  deriving (Read, Show, Eq)

type Depth = Int
type HorPos = Int
type Aim = Int
type Pos = (Depth, HorPos)
type Pos2 = (Depth, HorPos, Aim)

day2 = L.Solution 2 parse part1 part2

parse :: String -> [Direction]
parse = map (read . cap) . lines
  where cap (x:xs) = C.toUpper x : xs
        cap [] = []

part1 :: [Direction] -> Int
part1 xs = d * h
  where (d,h) = applyDirections (0,0) xs

applyDirections :: Pos -> [Direction] -> Pos
applyDirections = foldr applyDirection
  where
    applyDirection :: Direction -> Pos -> Pos
    applyDirection (Forward n) (d,h) = (d,h+n)
    applyDirection (Up n) (d,h) = (d-n,h)
    applyDirection (Down n) (d,h) = (d+n,h)

part2 :: [Direction] -> Int
part2 xs = d * h
  where (d,h,_) = applyDirections2 (0,0,0) xs

applyDirections2 :: Pos2 -> [Direction] -> Pos2
applyDirections2 = foldl applyDirection
  where
    applyDirection :: Pos2 -> Direction -> Pos2
    applyDirection (d,h,a) (Forward n)  = (d+(a*n),h+n,a)
    applyDirection (d,h,a) (Up n) = (d,h,a-n)
    applyDirection (d,h,a) (Down n) = (d,h,a+n)
