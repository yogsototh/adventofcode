import Test.Tasty
import Test.Tasty.HUnit

import qualified Day1

main :: IO ()
main = defaultMain $
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
