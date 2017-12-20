{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
{-| description:

--- Day 17: Spinlock ---

Suddenly, whirling in the distance, you notice what looks like a massive,
pixelated hurricane: a deadly spinlock. This spinlock isn't just consuming
computing power, but memory, too; vast, digital mountains are being ripped from
the ground and consumed by the vortex.

If you don't move quickly, fixing that printer will be the least of your
problems.

This spinlock's algorithm is simple but efficient, quickly consuming everything
in its path. It starts with a circular buffer containing only the value 0, which
it marks as the current position. It then steps forward through the circular
buffer some number of steps (your puzzle input) before inserting the first new
value, 1, after the value it stopped on. The inserted value becomes the current
position. Then, it steps forward from there the same number of steps, and
wherever it stops, inserts after it the second new value, 2, and uses that as
the new current position again.

It repeats this process of stepping forward, inserting a new value, and using
the location of the inserted value as the new current position a total of 2017
times, inserting 2017 as its final operation, and ending with a total of 2018
values (including 0) in the circular buffer.

For example, if the spinlock were to step 3 times per insert, the circular
buffer would begin to evolve like this (using parentheses to mark the current
position after each iteration of the algorithm):

- (0), the initial state before any insertions.
- 0 (1): the spinlock steps forward three times (0, 0, 0), and then inserts the
  first value, 1, after it. 1 becomes the current position.
- 0 (2) 1: the spinlock steps forward three times (0, 1, 0), and then inserts
  the second value, 2, after it. 2 becomes the current position.
- 0 2 (3) 1: the spinlock steps forward three times (1, 0, 2), and then inserts
  the third value, 3, after it. 3 becomes the current position.

And so on:

- 0  2 (4) 3  1
- 0 (5) 2  4  3  1
- 0  5  2  4  3 (6) 1
- 0  5 (7) 2  4  3  6  1
- 0  5  7  2  4  3 (8) 6  1
- 0 (9) 5  7  2  4  3  8  6  1

Eventually, after 2017 insertions, the section of the circular buffer near the
last insertion looks like this:

1512  1134  151 (2017) 638  1513  851

Perhaps, if you can identify the value that will ultimately be after the last
value written (2017), you can short-circuit the spinlock. In this example, that
would be 638.

What is the value after 2017 in your completed circular buffer?


--- Part Two ---

The spinlock does not short-circuit. Instead, it gets more angry. At least, you
assume that's what happened; it's spinning significantly faster than it was a
moment ago.

You have good news and bad news.

The good news is that you have improved calculations for how to stop the
spinlock. They indicate that you actually need to identify the value after 0 in
the current state of the circular buffer.

The bad news is that while you were determining this, the spinlock has just
finished inserting its fifty millionth value (50000000).

What is the value after 0 the moment 50000000 is inserted?

|-}

module Day17 where

import           Protolude

import qualified Data.Text as T

input :: Int
input = 303
testInput :: Int
testInput = 3

data Buffer = Buffer { elems :: [Int]
                     , pos   :: Int
                     } deriving (Show)

nextPreStep :: Int -> Buffer -> Buffer
nextPreStep nbSteps b@Buffer{..} =
  b { pos = (pos + nbSteps) `rem` length elems }

showBuffer :: Buffer -> Text
showBuffer Buffer{..} =
  zip  [0..] elems
  & map (\(i,v) -> if i == pos
                     then "(" <> show v <> ") "
                     else show v <> " ")
  & T.concat

nextStep :: Int -> Int -> Buffer -> Buffer
nextStep nbSteps roundNumber b =
  b
  & nextPreStep nbSteps
  & addElem roundNumber
  -- & \x -> trace (showBuffer x) (addElem roundNumber x)

addElem :: Int -> Buffer -> Buffer
addElem roundNumber b@Buffer {..} =
  b { elems = newelems
    , pos = pos + 1 `rem` length newelems
    }
  where
    newelems = take (pos + 1) elems
               ++ [roundNumber]
               ++ drop (pos + 1) elems

solution1 :: Int -> Int -> IO (Maybe Int)
solution1 nbOcc nbSteps = go initState 0
  where
    initState = Buffer [0] 0
    go st n
      | n == nbOcc = st & elems & elemAt (1 + (pos st)) & return
      | otherwise = do
            -- print st
            -- putText $ "> " <> showBuffer st
            go (nextStep nbSteps (n+1) st) (n+1)
    elemAt :: Int -> [a] -> Maybe a
    elemAt 0 (x:xs) = Just x
    elemAt n (_:xs) = elemAt (n-1) xs
    elemAt _ _      = Nothing

data Buf2 = Buf2 { position :: Int
                 , size :: Int
                 , lastSol :: Int
                 }

solution2 :: Int -> Int -> Int
solution2 nbOcc nbSteps = go initState 1
  where
    initState = Buf2 0 1 0
    go st@Buf2{..} n
      | n == nbOcc = lastSol
      | (position + nbSteps) `rem` size == 0 =
          traceShow n $ go (st { position = 1
                               , lastSol = n
                               , size = size + 1
                               })
                           (n+1)
      | otherwise = go (st { position = (1 + position + nbSteps) `rem` size
                           , size = size + 1 })
                       (n+1)
