{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Text.PrettyPrint hiding ((<>))

import qualified Day1
import qualified Day5
import qualified Day6

showSol :: [Char] -> Doc -> IO ()
showSol txt d = putText . toS . render $
  text ("  " <> txt <> ":")
  <+> d

main :: IO ()
main = do
  putText "Day 1:"
  input1 <- Day1.ex1code
  showSol "Solution 1" (int (Day1.solution1 input1))
  showSol "Solution 2" (int (Day1.solution2 input1))
  putText "Day 5:"
  input5 <- Day5.parseInput
  sol5 <- Day5.solution2 input5
  showSol "Solution 2" (int sol5)
  putText "Day 6:"
  input6_1 <- Day6.parseInput
  sol6_1 <- Day6.solution1 input6_1
  showSol "Solution 1" (int sol6_1)
  input6_2 <- Day6.parseInput
  sol6_2 <- Day6.solution2 input6_2
  showSol "Solution 2" (int sol6_2)
