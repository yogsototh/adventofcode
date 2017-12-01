{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Text.PrettyPrint hiding ((<>))

import qualified Day1

showSol :: [Char] -> Doc -> IO ()
showSol txt d = putText . toS . render $
  text ("  " <> txt <> ":")
  <+> d

main :: IO ()
main = do
  putText "Day 1:"
  showSol "Solution 1" (int (Day1.solution1 Day1.ex1code))
  showSol "Solution 2" (int (Day1.solution2 Day1.ex1code))
