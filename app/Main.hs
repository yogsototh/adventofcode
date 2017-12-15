{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Text.PrettyPrint hiding ((<>))
import System.Environment (getArgs)
import qualified Data.Map as Map

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15

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
solutions = Map.fromList [(["01"], day01)
                         ,(["02"], day02)
                         ,(["03"], day03)
                         ,(["04"], day04)
                         ,(["05"], day05)
                         ,(["06"], day06)
                         ,(["07"], day07)
                         ,(["08"], day08)
                         ,(["09"], day09)
                         ,(["10"], day10)
                         ,(["11"], day11)
                         ,(["12"], day12)
                         ,(["13"], day13)
                         ,(["14"], day14)
                         ,(["15"], day15)
                         ]


-- | Show ZERO in case of failure
mint = int . fromMaybe 0
-- | Show ZERO in case of failure
minteger = integer . fromMaybe 0

day01 :: IO ()
day01 = do
  putText "Day 01:"
  input1 <- Day01.ex1code
  showSol "Solution 1" (int (Day01.solution1 input1))
  showSol "Solution 2" (int (Day01.solution2 input1))

day02 :: IO ()
day02 = do
  putText "Day 02:"
  input <- Day02.parseInput
  let sol1 = maybe 0 Day02.solution1 input
  showSol "Solution 1" (integer sol1)
  let sol2 = maybe 0 Day02.solution2 input
  showSol "Solution 2" (integer sol2)

day03 :: IO ()
day03 = do
  putText "Day 03:"
  showSol "Solution 1" (minteger (Day03.solution1 Day03.input))
  showSol "Solution 2" (minteger (Day03.solution2 (fromIntegral Day03.input)))

day04 :: IO ()
day04 = do
  putText "Day 04:"
  input <- Day04.parseInput
  let sol1 = Day04.solution1 input
  showSol "Solution 1" (int sol1)
  let sol2 = Day04.solution2 input
  showSol "Solution 2" (int sol2)

day05 :: IO ()
day05 = do
  putText "Day 05:"
  input5 <- Day05.parseInput
  sol5 <- Day05.solution2 input5
  showSol "Solution 2" (int sol5)

day06 :: IO ()
day06 = do
  putText "Day 06:"
  input6_1 <- Day06.parseInput
  sol6_1 <- Day06.solution1 input6_1
  showSol "Solution 1" (int sol6_1)
  input6_2 <- Day06.parseInput
  sol6_2 <- Day06.solution2 input6_2
  showSol "Solution 2" (int sol6_2)

day07 :: IO ()
day07 = do
  putText "Day 07:"
  input <- Day07.parseInput
  let sol_1 = Day07.rootOf input
  showSol "Solution 1" (text (toS (maybe "" Day07.name sol_1)))
  let sol_2 = Day07.solution2 input
  showSol "Solution 2" (int (maybe 0 snd sol_2))

day08 :: IO ()
day08 = do
  putText "Day 08:"
  input <- Day08.parseInput
  let sol1 = Day08.solution1 input
  showSol "Solution 1" (int sol1)
  let sol2 = Day08.solution2 input
  showSol "Solution 2" (int sol2)

day09 :: IO ()
day09 = do
  putText "Day 09:"
  input <- Day09.parseInput
  let sol1 = Day09.solution1 input
  showSol "Solution 1" (int sol1)
  let sol2 = Day09.solution2 input
  showSol "Solution 2" (int sol2)

day10 :: IO ()
day10 = do
  putText "Day 10:"
  let sol1 = Day10.solution1 Day10.input
  showSol "Solution 1" (int sol1)
  input2 <- Day10.parseInput2
  showSol "Solution 2" (text (toS (Day10.solution2 input2)))

day11 :: IO ()
day11 = do
  putText "Day 11:"
  input <- Day11.parseInput
  let sol1 = Day11.solution1 input
  showSol "Solution 1" (int sol1)

day12 :: IO ()
day12 = do
  putText "Day 12:"
  input <- Day12.parseInput
  let sol1 = fmap Day12.solution1 input
  showSol "Solution 1" (int (fromMaybe 0 sol1))
  let sol2 = fmap Day12.solution2 input
  showSol "Solution 2" (int (fromMaybe 0 sol2))

day13 :: IO ()
day13 = do
  putText "Day 13:"
  input <- Day13.input
  let sol1 = fmap Day13.solution1 input
  showSol "Solution 1" (int (fromMaybe 0 sol1))
  input2 <- Day13.parseInput
  let sol2 = Day13.solution2 =<< input2
  showSol "Solution 2" (int (fromMaybe 0 sol2))

day14 :: IO ()
day14 = do
  putText "Day 14:"
  let sol1 = Day14.solution1 Day14.input
  showSol "Solution 1" (int (fromMaybe 0 sol1))
  sol2 <- Day14.solution2 Day14.input
  showSol "Solution 2" (int sol2)

day15 :: IO ()
day15 = do
  putText "Day 15:"
  let sol1 = Day15.solution1 Day15.input
  showSol "Solution 1" (int sol1)
  let sol2 = Day15.solution2 Day15.input
  showSol "Solution 1" (int sol2)
