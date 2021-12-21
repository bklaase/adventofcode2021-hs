{-# LANGUAGE RecordWildCards #-}

module Solutions.Day14 where

import qualified AoC.Lib as L
import Data.List (find, foldl1')
import Data.Maybe (fromJust)
import qualified Data.Map as M

day14 = L.Solution 14 parse part1 part2

-- types
type Element = Char
data Rule = Rule {p1 :: Element, p2 :: Element, ins :: Element} deriving Show
data Input = Input {template :: [Element], rules :: [Rule] } deriving Show

-- parsing
parseRule :: String -> Rule
parseRule s = Rule (head s) (s !! 1) (s !! 6)

parse :: String -> Input
parse s = Input tpl rs
  where
    ls = lines s
    tpl = head ls
    rs = map parseRule $ tail $ tail ls

-- solutions

getRule :: (Element, Element) -> [Rule] -> Rule
getRule (e1, e2) rs = fromJust $ find filterRule rs
  where
    filterRule Rule {..} = p1 == e1 && p2 == e2

applyRulesToInput :: [Rule] -> [Element] -> [Element]
applyRulesToInput rules (x1:x2:xs) = x1 : insert : applyRulesToInput rules (x2:xs)
  where
    Rule{ins=insert} = getRule (x1,x2) rules
applyRulesToInput _ xs = xs

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f n xs = results !! n
  where results = iterate f xs

countMapFromElements :: [Element] -> M.Map Element Int
countMapFromElements es = addToMap M.empty es
  where
    addToMap :: M.Map Element Int -> [Element] -> M.Map Element Int
    addToMap m (e:es) = addToMap (M.insertWith (+) e 1 m) es
    addToMap m _ = m

findExtreme :: (a -> a -> Bool) -> [a] -> a
findExtreme f = foldl1' p
  where
    p a b
      | f a b = a
      | otherwise = b

part1 :: Input -> Int
part1 input = foundMax - foundMin
  where
    counts = countMapFromElements elements
    elements = applyNTimes (applyRulesToInput (rules input)) 10 (template input)
    foundMin = snd $ findExtreme (compSnds (<)) (M.toList counts)
    foundMax = snd $ findExtreme (compSnds (>)) (M.toList counts)
    compSnds f a b = f (snd a)  (snd b)


part2 :: Input -> Int
part2 xs = 0
