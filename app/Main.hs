{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Text.PrettyPrint hiding ((<>))
import System.Environment (getArgs)
import qualified Data.Map as Map

import qualified Day1
import qualified Day2
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

showSol :: [Char] -> Doc -> IO ()
showSol txt d = putText . toS . render $
  text ("  " <> txt <> ":")
  <+> d

main :: IO ()
main = do
  args <- getArgs
  fromMaybe (traverse_ snd (Map.toAscList solutions))
            (Map.lookup args solutions)

solutions :: Map [[Char]] (IO ())
solutions = Map.fromList [(["1"], day1)
                         ,(["2"], day2)
                         ,(["5"], day5)
                         ,(["6"], day6)
                         ,(["7"], day7)
                         ,(["8"], day8)
                         ,(["9"], day9)
                         ]

day1 :: IO ()
day1 = do
  putText "Day 1:"
  input1 <- Day1.ex1code
  showSol "Solution 1" (int (Day1.solution1 input1))
  showSol "Solution 2" (int (Day1.solution2 input1))

day2 :: IO ()
day2 = do
  putText "Day 2:"
  input <- Day2.parseInput
  let sol1 = maybe 0 Day2.solution1 input
  showSol "Solution 1" (integer sol1)
  let sol2 = maybe 0 Day2.solution2 input
  showSol "Solution 2" (integer sol2)

day5 :: IO ()
day5 = do
  putText "Day 5:"
  input5 <- Day5.parseInput
  sol5 <- Day5.solution2 input5
  showSol "Solution 2" (int sol5)

day6 :: IO ()
day6 = do
  putText "Day 6:"
  input6_1 <- Day6.parseInput
  sol6_1 <- Day6.solution1 input6_1
  showSol "Solution 1" (int sol6_1)
  input6_2 <- Day6.parseInput
  sol6_2 <- Day6.solution2 input6_2
  showSol "Solution 2" (int sol6_2)

day7 :: IO ()
day7 = do
  putText "Day 7:"
  input <- Day7.parseInput
  let sol_1 = Day7.rootOf input
  showSol "Solution 1" (text (toS (maybe "" Day7.name sol_1)))
  let sol_2 = Day7.solution2 input
  showSol "Solution 2" (int (maybe 0 snd sol_2))

day8 :: IO ()
day8 = do
  putText "Day 8:"
  input <- Day8.parseInput
  let sol1 = Day8.solution1 input
  showSol "Solution 1" (int sol1)
  let sol2 = Day8.solution2 input
  showSol "Solution 2" (int sol2)

day9 :: IO ()
day9 = do
  putText "Day 9:"
  sol1 <- Day9.solution1
  showSol "Solution 1" (int sol1)
  sol2 <- Day9.solution2
  showSol "Solution 2" (int sol2)
