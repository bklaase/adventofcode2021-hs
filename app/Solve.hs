module Main where

import System.IO
import Text.Read
import qualified AoC.Lib as L
import Solutions.Day1 (day1)
import Solutions.Day2 (day2)
import Solutions.Day3 (day3)

main :: IO ()
main = 
  putStrLn "Press ctrl-c to quit." >> loop
  where loop =
          putStrLn "Enter a day number to get its results..." >>
          askForType >>= solve >>
          putStrLn [] >> loop

solve :: Int -> IO ()
solve n =
  case n of
    1 -> L.solve day1
    2 -> L.solve day2
    3 -> L.solve day3
    otherwise -> putStrLn "no solution found..."

askForType :: Read a => IO a
askForType = 
  getLine >>= returnOrRepeat . readMaybe
  where returnOrRepeat Nothing =
          putStrLn "Invalid input. Try again..." >> askForType
        returnOrRepeat (Just i) = return i
