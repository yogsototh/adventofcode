{-# LANGUAGE OverloadedStrings #-}
import Protolude

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad (when)

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

main :: IO ()
main = defaultMain $
  testGroup "Advent Of Code 2017"
  [ testDay01
  , testDay02
  , testDay03
  , testDay04
  , testDay05
  , testDay06
  , testDay07
  , testDay08
  , testDay09
  , testDay10
  , testDay11
  , testDay12
  , testDay13
  ]

testDay01 =
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

testDay02 =
  testGroup "Day 2"
  [ testGroup "Solution 1"
    [ testCase "Example" $
      Day02.solution1 [[5,1,9,5],[7,5,3],[2,4,6,8]] @?= 18]
  , testGroup "Solution 2"
    [testCase "Example" $
      Day02.solution2 [[5,9,2,8],[9,4,7,3],[3,8,6,5]] @?= 9
    ]
  ]

testDay03 =
  testGroup "Day 3"
  [ testGroup "Solution 1"
    [ testCase "1" $ Day03.returnPathLength 1 @?= Just 0
    , testCase "12" $ Day03.returnPathLength 12 @?= Just 3
    , testCase "23" $ Day03.returnPathLength 23 @?= Just 2
    , testCase "1024" $ Day03.returnPathLength 1024 @?= Just 31
    ]
  , testGroup "Solution 2"
    [ testCase "2" $ Day03.solution2 2 @?= Just 2
    , testCase "3" $ Day03.solution2 3 @?= Just 4
    , testCase "4" $ Day03.solution2 4 @?= Just 4
    , testCase "6" $ Day03.solution2 6 @?= Just 10
    , testCase "747" $ Day03.solution2 747 @?= Just 747
    , testCase "748" $ Day03.solution2 748 @?= Just 806
    , testCase "800" $ Day03.solution2 800 @?= Just 806
    , testCase "805" $ Day03.solution2 805 @?= Just 806
    , testCase "806" $ Day03.solution2 806 @?= Just 806
    ]
  ]

testDay04 =
  testGroup "Day 4"
  [ testGroup "Solution 1"
    [ testCase "1" $ Day04.solution1 (Day04.parseTxt testTxt) @?= 2 ]
  , testGroup "Solution 2"
    [ testCase "2" $ Day04.solution2 (Day04.parseTxt testTxt2) @?= 3 ]
  ]
  where
    testTxt = "aa bb cc dd ee\n\
              \aa bb cc dd aa\n\
              \aa bb cc dd aaa\n"
    testTxt2 = "abcde fghij\n\
               \abcde xyz ecdab\n\
               \a ab abc abd abf abj\n\
               \iiii oiii ooii oooi oooo\n\
               \oiii ioii iioi iiio\n"

testDay05 =
  testGroup "Day 5"
  [ testCaseSteps "example problem 1" $ \step -> do
      step "Loading input"
      input <- Day05.testArray
      step "Running solution 1"
      sol1 <- Day05.solution1 input
      when (sol1 /= 5)
        (assertFailure "Should be 5 steps")
  ,  testCaseSteps "example problem 2" $ \step -> do
      step "Loading input"
      input <- Day05.testArray
      step "Running solution 2"
      sol2 <- Day05.solution2 input
      when (sol2 /= 10)
        (assertFailure "Day 6 solution 2 on the example should be 4")
  ]

testDay06 =
  testGroup "Day 6"
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

testDay07 =
  testGroup "Day 7"
    [ testCaseSteps "example problem 1" $ \step -> do
        step "Running solution 1"
        let input = Day07.testNodes
        let sol1 = maybe "" Day07.name (Day07.rootOf input)
        when (sol1 /= "tknk") (assertFailure "The root should be tknk")
    , testCase "example on solution 2" $
       maybe 0 snd (Day07.solution2 Day07.testNodes) @?= 60
    ]

testDay08 =
  testGroup "Day 8"
    [ testCase "example problem 1" $
        Day08.solution1 Day08.testInstructions @?= 1
    , testCase "example problem 1" $
        Day08.solution2 Day08.testInstructions @?= 10
    ]

testDay09 =
  testGroup "Day 9"
  [ testGroup "Solution 1"
    [ check1 "{}" 1
    , check1 "{{{}}}" 6
    , check1 "{{},{}}" 5
    , check1 "{{{},{},{{}}}}" 16
    , check1 "{<a>,<a>,<a>,<a>}" 1
    , check1 "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    , check1 "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    , check1 "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
    ]
  , testGroup "Solution 2"
    [ check2 "<>" 0
    , check2 "<random characters>" 17
    , check2 "<<<<>" 3
    , check2 "<{!>}>" 2
    , check2 "<!!>" 0
    , check2 "<!!!>>" 0
    , check2 "<{o\"i!a,<{i<a>" 10
    ]
  ]
  where
    check1 txt v = testCase (toS txt) (Day09.solution1 txt @?= v)
    check2 txt v = testCase (toS txt) (Day09.solution2 ("{" <> txt <> "}") @?= v)

testDay10 =
  testGroup "Day 10"
  [ testGroup "Solution 1"
    [ testCase "example 1" $ Day10.solution1 Day10.testInput @?= 12 ]
  , testGroup "Solution 2"
  [check2 "" "a2582a3a0e66e6e86e3812dcb672a272"
  , check2 "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
  , check2 "1,2,3" "3efbe78a8d82f29979031a4aa0b16a9d"
  , check2 "1,2,4" "63960835bcdc130f0b66d7ff4f6a5a8e"
  ]
  ]
  where
    check2 txt v = testCase (toS ("\"" <> txt <> "\""))
                            (Day10.solution2 txt @?= v)

testDay11 =
  testGroup "Day 11"
  [ testGroup "Solution 1"
      [ check1 "ne,ne,ne" 3
      , check1 "ne,ne,sw,sw" 0
      , check1 "ne,ne,s,s" 2
      , check1 "se,sw,se,sw,sw" 3
      ]
  , testGroup "Solution 2"
    [ check2 "ne,ne,ne" 3
    , check2 "ne,ne,sw,sw" 2
    , check2 "ne,ne,s,s" 2
    , check2 "se,sw,se,sw,sw" 3
    ]
    ]
  where
    check1 txt v = testCase (toS txt)
                        (Day11.solution1 (Day11.parseTxt txt) @?= v)
    check2 txt v = testCase (toS txt)
                        (Day11.solution2 (Day11.parseTxt txt) @?= v)


testDay12 =
  testGroup "Day 12"
  [ testGroup "Solution 1"
    [ testCase "Example" $
      fmap Day12.solution1 (Day12.parseTxt Day12.testTxt) @?= Just 6
    ]
  , testGroup "Solution 2"
    [ testCase "Example" $
      fmap Day12.solution2 (Day12.parseTxt Day12.testTxt) @?= Just 2
    ]
  ]

testDay13 =
  testGroup "Day 13"
  [ testGroup "Solution 1"
    [ testCase "Example" $
      (Day13.solution1 . Day13.mkAppState) <$> Day13.parseTxt Day13.testInput
      @?= Just 24
    ]
  , testGroup "Solution 2"
    [ testCase "Example" $
       (Day13.solution2 =<< Day13.parseTxt Day13.testInput) @?= Just 10
    ]
  ]
