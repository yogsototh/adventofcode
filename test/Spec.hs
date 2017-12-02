import Test.Tasty
import Test.Tasty.HUnit

import qualified Day1
import qualified Day2

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
  ]
