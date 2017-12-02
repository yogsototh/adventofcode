{-# LANGUAGE NoImplicitPrelude #-}
module Day1 where

import Protolude

import Data.List (tail, foldl')
import Data.Char (ord)

ex1code :: IO [Char]
ex1code = toS <$> readFile "inputs/day1.txt"

sum :: Num a => [a] -> a
sum = foldl' (+) 0

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

solution1 :: [Char] -> Int
solution1 code = code
  & cycle
  & tail
  & zip code
  & filter (uncurry (==))
  & map (charToInt . fst)
  & foldl' (+) 0

solution2 :: [Char] -> Int
solution2 code =
  let n = length code `div` 2
  in code
     & cycle
     & drop n
     & zip code
     & filter (uncurry (==))
     & map (charToInt . fst)
     & foldl' (+) 0
