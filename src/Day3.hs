{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
description:

--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

- Data from square 1 is carried 0 steps, since it's at the access port.
- Data from square 12 is carried 3 steps, such as: down, left, left.
- Data from square 23 is carried only 2 steps: up twice.
- Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
|-}
module Day2 where

import Protolude
import qualified Control.Foldl as F

import Data.List (words,lines)

input = 265149


blocks l (previ,prevx,prevy) =
  concat [[(previ + 1 + n, prevx + 1, prevy + n) | n <- [0..l]]
         , [(previ + 2 + l + n, prevx - n, prevy + l) | n <- [0..l]]
         , [(previ + 3 + 2*l + n, prevx - l, prevy + l - n - 1) | n <- [0..l]]
         , [(previ + 4 + 3*l + n, prevx - l + n + 1, prevy - 1) | n <- [0..l]]
         ]

block1 = [ (2,1,0)
         , (3,1,1)
         , (4,0,1)
         , (5,-1,1)
         , (6,-1,0)
         , (7,-1,-1)
         , (8,0,-1)
         , (9,1,-1)
         ]
spiral = (1,0,0):concatMap (\n -> blocks (n+1) ((n+1)^2,n `div` 2,- (n `div` 2))) [0,2..]

returnPathLength i =
  drop (i - 1) spiral
  & head
  & fmap (\(_,x,y) -> abs x + abs y)


