{-# LANGUAGE NoImplicitPrelude #-}
{-|
description:

--- Day 2: Corruption Checksum ---

As you walk through the door, a glowing humanoid shape yells in your direction. "You there! Your state appears to be idle. Come help us repair the corruption in this spreadsheet - if we take another millisecond, we'll have to display an hourglass cursor!"

The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

For example, given the following spreadsheet:

5 1 9 5
7 5 3
2 4 6 8
The first row's largest and smallest values are 9 and 1, and their difference is 8.
The second row's largest and smallest values are 7 and 3, and their difference is 4.
The third row's difference is 6.
In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

What is the checksum for the spreadsheet in your puzzle input?

Your puzzle answer was 51139.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---

"Great work; looks like we're on the right track after all. Here's a star for your effort." However, the program seems a little worried. Can programs be worried?

"Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."

It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.

For example, given the following spreadsheet:

5 9 2 8
9 4 7 3
3 8 6 5
In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
In the second row, the two numbers are 9 and 3; the result is 3.
In the third row, the result is 2.
In this example, the sum of the results would be 4 + 3 + 2 = 9.

What is the sum of each row's result in your puzzle input?
|-}
module Day2 where

import Protolude
import qualified Control.Foldl as F

import Data.List (foldl')

input = [
  [3458,3471,163,1299,170,4200,2425,167,3636,4001,4162,115,2859,130,4075,4269]
  , [2777,2712,120,2569,2530,3035,1818,32,491,872,113,92,2526,477,138,1360]
  , [2316,35,168,174,1404,1437,2631,1863,1127,640,1745,171,2391,2587,214,193]
  , [197,2013,551,1661,121,206,203,174,2289,843,732,2117,360,1193,999,2088]
  , [3925,3389,218,1134,220,171,1972,348,3919,3706,494,3577,3320,239,120,2508]
  , [239,947,1029,2024,733,242,217,1781,2904,2156,1500,3100,497,2498,3312,211]
  , [188,3806,3901,261,235,3733,3747,3721,267,3794,3814,3995,3004,915,4062,3400]
  , [918,63,2854,2799,178,176,1037,487,206,157,2212,2539,2816,2501,927,3147]
  , [186,194,307,672,208,351,243,180,619,749,590,745,671,707,334,224]
  , [1854,3180,1345,3421,478,214,198,194,4942,5564,2469,242,5248,5786,5260,4127]
  , [3780,2880,236,330,3227,1252,3540,218,213,458,201,408,3240,249,1968,2066]
  , [1188,696,241,57,151,609,199,765,1078,976,1194,177,238,658,860,1228]
  , [903,612,188,766,196,900,62,869,892,123,226,57,940,168,165,103]
  , [710,3784,83,2087,2582,3941,97,1412,2859,117,3880,411,102,3691,4366,4104]
  , [3178,219,253,1297,3661,1552,8248,678,245,7042,260,581,7350,431,8281,8117]
  , [837,80,95,281,652,822,1028,1295,101,1140,88,452,85,444,649,1247]
  ]

solution1 :: [[Integer]] -> Integer
solution1 code =
  map maxDiff code
  & sum

maxDiff :: [Integer] -> Integer
maxDiff = (\(mmax,mmin) -> fromMaybe 0 mmax - fromMaybe 0 mmin) .
          F.fold ((,) <$> F.maximum <*> F.minimum)

solution2 :: [[Integer]] -> Integer
solution2 code = map divisibleNumbers code & sum

divisibleNumbers :: [Integer] -> Integer
divisibleNumbers numbers =
  [ x `div` y | x <- numbers, y <- numbers, x>y, x `rem` y == 0]
  & headDef 0
