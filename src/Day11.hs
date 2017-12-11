{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
description:

--- Day 11: Hex Ed ---

Crossing the bridge, you've barely reached the other side of the stream when a
program comes up to you, clearly in distress. "It's my child process," she says,
"he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be
found to the north, northeast, southeast, south, southwest, and northwest:

  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \

You have the path the child process took. Starting where he started, you need to
determine the fewest number of steps required to reach him. (A "step" means to
move from the hex you are in to any adjacent hex.)

For example:

- ne,ne,ne is 3 steps away.
- ne,ne,sw,sw is 0 steps away (back where you started).
- ne,ne,s,s is 2 steps away (se,se).
- se,sw,se,sw,sw is 3 steps away (s,s,sw).

--- Part Two ---

How many steps away is the furthest he ever got from his starting position?

|-}

module Day11 where

import           Protolude

import qualified Data.Text as T
import Data.List (scanl',last)

data Move = NW | N | NE | SE | S | SW deriving (Show, Eq, Ord)


parseInput :: IO [Move]
parseInput = parseTxt <$> readFile "inputs/day11.txt"

parseTxt :: Text -> [Move]
parseTxt str = str & T.strip & T.splitOn "," & map strToMove
  where
    strToMove "n"  = N
    strToMove "nw" = NW
    strToMove "ne" = NE
    strToMove "s"  = S
    strToMove "sw" = SW
    strToMove "se" = SE

data Coord = Coord Int Int Int deriving (Show, Eq, Ord)

direction :: Move -> Coord
direction N  = Coord 0    1    (-1)
direction S  = Coord 0    (-1) 1
direction NE = Coord 1    0    (-1)
direction SW = Coord (-1) 0    1
direction NW = Coord (-1) 1    0
direction SE = Coord 1    (-1) 0

sumCoord :: Coord -> Coord -> Coord
sumCoord (Coord x1 y1 z1) (Coord x2 y2 z2) =
  Coord (x1 + x2) (y1 + y2) (z1 + z2)

origin :: Coord
origin = Coord 0 0 0

dist :: Coord -> Int
dist (Coord x1 y1 z1) =
  (abs x1 + abs y1 + abs z1) `div` 2

solution1 :: [Move] -> Int
solution1 moves =
  map direction moves
  & foldl' sumCoord origin
  & dist

solution2 :: [Move] -> Int
solution2 moves =
  map direction moves
  & scanl' sumCoord origin
  & map dist
  & maximum
