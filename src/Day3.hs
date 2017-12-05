{-# LANGUAGE NoImplicitPrelude #-}
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


--- Part Two ---

As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
What is the first value written that is larger than your puzzle input?



|-}
module Day3 where

import Protolude
import qualified Control.Foldl as F

import Data.List (words,lines)
import qualified Data.Map as Map

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


-- Solution 2

type Spiral = Map (Int,Int) Integer

spiralDebug = go Map.empty (0,0)
  where
    go s coord = let (nextspiral,nextcoord,val) = nextPoint s coord
                     neigh = neighbor s coord
                 in (val,nextcoord,neigh):go nextspiral nextcoord
spiral2 = map (\(x,_,_) -> x) spiralDebug

solution2 = head $ dropWhile (< 265149) spiral2

neighbor :: Spiral -> (Int,Int) -> [Maybe Integer]
neighbor spiral (x,y) =
  [ (xn,yn) `Map.lookup` spiral |yn <- [y+1,y,y-1], xn <- [x-1,x,x+1]]

sumMaybes :: [Maybe Integer] -> Integer
sumMaybes l = max 1 $ maybe 1 sum (sequence (filter isJust l))

nextPoint :: Spiral -> (Int,Int) -> (Spiral,(Int,Int),Integer)
nextPoint spiral (x,y) =
  let neigh = neighbor spiral (x,y)
      s     = sumMaybes neigh
      coord = next neigh (x,y)
      newSpiral = Map.insert (x,y) s spiral
  in (newSpiral,coord,s)

next :: [Maybe Integer] -> (Int,Int) -> (Int,Int)

next [_      ,Nothing,_
     ,Nothing,_      ,Nothing
     ,_      ,Nothing,_      ] (x,y) = (x+1,y)

next [_     ,Nothing,_
     ,Just _, _     ,_
     ,_     ,_      ,_       ] (x,y) = (x,y+1)

next [_      ,_      ,_
     ,Nothing,_      ,_
     ,_      , Just _, _] (x,y) = (x-1,y)

next [_      ,_      ,_
     ,_      ,_      ,Just _
     ,_      ,Nothing,     _] (x,y) = (x,y-1)

next [_      ,Just _, _
     ,_      ,_      ,Nothing
     ,_      ,_      ,_      ] (x,y) = (x+1,y)
