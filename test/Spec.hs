{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad (when)

import qualified Day1
import qualified Day2
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day10


main :: IO ()
main = defaultMain $
  testGroup "Advent Of Code 2017"
  [
  testGroup "Day 1"
  [ testGroup "solution 1"
    [ testCase "1122 is 3" $ Day1.solution1 "1122" @?= 3
    , testCase "1111 is 4" $ Day1.solution1 "1111" @?= 4
    , testCase "1234 is 0" $ Day1.solution1 "1234" @?= 0
    , testCase "91212129 is 9"  $ Day1.solution1 "91212129" @?= 9
    ]
  , testGroup "solution 2"
    [ testCase "1212 is 6" $ Day1.solution2 "1212" @?= 6
    , testCase "1221 is 0" $ Day1.solution2 "1221" @?= 0
    , testCase "123425 is 0" $ Day1.solution2 "123425" @?= 4
    , testCase "123123 is 12" $ Day1.solution2 "123123" @?= 12
    , testCase "12131415 is 4" $ Day1.solution2 "12131415" @?= 4
    ]
  ]
  , testGroup "Day 2"
    [ testCase "example problem 1" $
      Day2.solution1 [[5,1,9,5],[7,5,3],[2,4,6,8]] @?= 18
    ,  testCase "example problem 2" $
      Day2.solution2 [[5,9,2,8],[9,4,7,3],[3,8,6,5]] @?= 9
    ]
  , testGroup "Day 5"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Loading input"
        input <- Day5.testArray
        step "Running solution 1"
        sol1 <- Day5.solution1 input
        when (sol1 /= 5) (assertFailure "Should be 5 steps")
    ,  testCaseSteps "example problem 2" $ \step -> do
        step "Loading input"
        input <- Day5.testArray
        step "Running solution 2"
        sol2 <- Day5.solution2 input
        when (sol2 /= 10) (assertFailure "Day 6 solution 2 on the example should be 4")
    ]
  , testGroup "Day 6"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Loading input"
        input <- Day6.testArray
        step "Running solution 1"
        sol1 <- Day6.solution1 input
        when (sol1 /= 5) (assertFailure "Should be 5 steps")
    ,  testCaseSteps "example problem 2" $ \step -> do
        step "Loading input"
        input <- Day6.testArray
        step "Running solution 2"
        sol2 <- Day6.solution2 input
        when (sol2 /= 4) (assertFailure "Day 6 solution 2 on the example should be 4")
    ]
  , testGroup "Day 7"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Running solution 1"
        let input = Day7.testNodes
        let sol1 = maybe "" Day7.name (Day7.rootOf input)
        when (sol1 /= "tknk") (assertFailure "The root should be tknk")
    , testCase "example on solution 2" $
       maybe 0 snd (Day7.solution2 Day7.testNodes) @?= 60
    ]
  , testGroup "Day 8"
    [ testCase "example problem 1" $
        Day8.solution1 Day8.testInstructions @?= 1
    , testCase "example problem 1" $
        Day8.solution2 Day8.testInstructions @?= 10
    ]
  , testGroup "Day 10"
    [ testCase "example 1" $
        Day10.solution1 Day10.testInput @?= 12
    ]
  ]
