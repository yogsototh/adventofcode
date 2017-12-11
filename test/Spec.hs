{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad (when)

import qualified Day01
import qualified Day02
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day10
import qualified Day11


main :: IO ()
main = defaultMain $
  testGroup "Advent Of Code 2017"
  [
  testGroup "Day 1"
  [ testGroup "solution 1"
    [ testCase "1122 is 3" $ Day01.solution1 "1122" @?= 3
    , testCase "1111 is 4" $ Day01.solution1 "1111" @?= 4
    , testCase "1234 is 0" $ Day01.solution1 "1234" @?= 0
    , testCase "91212129 is 9"  $ Day01.solution1 "91212129" @?= 9
    ]
  , testGroup "solution 2"
    [ testCase "1212 is 6" $ Day01.solution2 "1212" @?= 6
    , testCase "1221 is 0" $ Day01.solution2 "1221" @?= 0
    , testCase "123425 is 0" $ Day01.solution2 "123425" @?= 4
    , testCase "123123 is 12" $ Day01.solution2 "123123" @?= 12
    , testCase "12131415 is 4" $ Day01.solution2 "12131415" @?= 4
    ]
  ]
  , testGroup "Day 2"
    [ testCase "example problem 1" $
      Day02.solution1 [[5,1,9,5],[7,5,3],[2,4,6,8]] @?= 18
    ,  testCase "example problem 2" $
      Day02.solution2 [[5,9,2,8],[9,4,7,3],[3,8,6,5]] @?= 9
    ]
  , testGroup "Day 5"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Loading input"
        input <- Day05.testArray
        step "Running solution 1"
        sol1 <- Day05.solution1 input
        when (sol1 /= 5) (assertFailure "Should be 5 steps")
    ,  testCaseSteps "example problem 2" $ \step -> do
        step "Loading input"
        input <- Day05.testArray
        step "Running solution 2"
        sol2 <- Day05.solution2 input
        when (sol2 /= 10) (assertFailure "Day 6 solution 2 on the example should be 4")
    ]
  , testGroup "Day 6"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Loading input"
        input <- Day06.testArray
        step "Running solution 1"
        sol1 <- Day06.solution1 input
        when (sol1 /= 5) (assertFailure "Should be 5 steps")
    ,  testCaseSteps "example problem 2" $ \step -> do
        step "Loading input"
        input <- Day06.testArray
        step "Running solution 2"
        sol2 <- Day06.solution2 input
        when (sol2 /= 4) (assertFailure "Day 6 solution 2 on the example should be 4")
    ]
  , testGroup "Day 7"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Running solution 1"
        let input = Day07.testNodes
        let sol1 = maybe "" Day07.name (Day07.rootOf input)
        when (sol1 /= "tknk") (assertFailure "The root should be tknk")
    , testCase "example on solution 2" $
       maybe 0 snd (Day07.solution2 Day07.testNodes) @?= 60
    ]
  , testGroup "Day 8"
    [ testCase "example problem 1" $
        Day08.solution1 Day08.testInstructions @?= 1
    , testCase "example problem 1" $
        Day08.solution2 Day08.testInstructions @?= 10
    ]
  , testGroup "Day 10"
    [ testCase "example 1" $
        Day10.solution1 Day10.testInput @?= 12
    , testCase "solution 2 empty" $
        Day10.solution2 "" @?= "a2582a3a0e66e6e86e3812dcb672a272"
    , testCase "solution 2 AoC 2017" $
        Day10.solution2 "AoC 2017" @?= "33efeb34ea91902bb2f59c9920caa6cd"
    , testCase "solution 2 1,2,3" $
        Day10.solution2 "1,2,3" @?= "3efbe78a8d82f29979031a4aa0b16a9d"
    , testCase "solution 2 1,2,4" $
        Day10.solution2 "1,2,4" @?= "63960835bcdc130f0b66d7ff4f6a5a8e"
    ]
  , testGroup "Day 11"
    [ testGroup "Solution 1"
      [ testCase "Example 1" $
          Day11.solution1 (Day11.parseTxt "ne,ne,ne") @?= 3
      , testCase "Example 2" $
         Day11.solution1 (Day11.parseTxt "ne,ne,sw,sw") @?= 0
      , testCase "Example 3" $
         Day11.solution1 (Day11.parseTxt "ne,ne,s,s") @?= 2
      , testCase "Example 4" $
         Day11.solution1 (Day11.parseTxt "se,sw,se,sw,sw") @?= 3
      ]
    ]
  ]
