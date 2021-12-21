module Solutions.Day4 where

import qualified AoC.Lib as L
import Data.List.Split
import Data.List (transpose)

day4 = L.Solution 4 parse part1 part2

newtype Board = Board {boardVals :: [[(Int, Bool)]]}
type Randoms = [Int]
type Game = (Randoms, [Board])


parseNumbers :: String -> Randoms
parseNumbers = map read . splitOn ","

parseBoards :: [String] -> [[[Int]]]
parseBoards = map (map (map read . words)) . splitOn ["",""]

parse :: String -> Game
parse s = (randoms, boards)
  where
    ls = lines s
    randoms = parseNumbers $ head ls
    boardVals = parseBoards $ tail $ tail ls
    boards = map (Board . map (`zip` repeat False)) boardVals

part1 :: Game -> Int
part1 xs = 0

part2 :: Game -> Int
part2 xs = 0
