{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-|
description:

--- Day 14: Disk Defragmentation ---

Suddenly, a scheduled job activates the system's disk defragmenter. Were the
situation different, you might sit and watch it for a while, but today, you just
don't have that kind of time. It's soaking up valuable system resources that are
needed elsewhere, and so the only option is to help it finish its task as soon
as possible.

The disk in question consists of a 128x128 grid; each square of the grid is
either free or used. On this disk, the state of the grid is tracked by the bits
in a sequence of knot hashes.

A total of 128 knot hashes are calculated, each corresponding to a single row in
the grid; each hash contains 128 bits which correspond to individual grid
squares. Each bit of a hash indicates whether that square is free (0) or used
(1).

The hash inputs are a key string (your puzzle input), a dash, and a number from
0 to 127 corresponding to the row. For example, if your key string were
flqrgnkx, then the first row would be given by the bits of the knot hash of
flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so
on until the last row, flqrgnkx-127.

The output of a knot hash is traditionally represented by 32 hexadecimal digits;
each of these digits correspond to 4 bits, for a total of 4 * 32 = 128 bits. To
convert to bits, turn each hexadecimal digit to its equivalent binary value,
high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111,
and so on; a hash that begins with a0c2017... in hexadecimal would begin with
10100000110000100000000101110000... in binary.

Continuing this process, the first 8 rows and columns for key flqrgnkx appear as
follows, using # to denote used squares, and . to denote free ones:

##.#.#..-->
.#.#.#.#
....#.#.
#.#.##.#
.##.#...
##..#..#
.#...#..
##.#.##.-->
|      |
V      V

In this example, 8108 squares are used across the entire 128x128 grid.

Given your actual key string, how many squares are used?

--- Part Two ---

Now, all the defragmenter needs to know is the number of regions. A region is a
group of used squares that are all adjacent, not including diagonals. Every used
square is in exactly one region: lone used squares form their own isolated
regions, while several adjacent squares all count as a single region.

In the example above, the following nine regions are visible, each marked with a
distinct digit:

11.2.3..-->
.1.2.3.4
....5.6.
7.8.55.9
.88.5...
88..5..8
.8...8..
88.8.88.-->
|      |
V      V

Of particular interest is the region marked 8; while it does not appear
contiguous in this small view, all of the squares marked 8 are connected when
considering the whole 128x128 grid. In total, in this example, 1242 regions are
present.

How many regions are present given your key string?

|-}
module Day14 where

import           Protolude

import qualified Day10

import           Data.Array.IO     (IOUArray)
import           Data.Array.MArray
import qualified Data.Text         as T

input :: Text
input = "hfdlxzhv"

testInput :: Text
testInput = "flqrgnkx"

-- solution1 :: Text -> Maybe Int
solution1 input =
  grid input
  & fmap (fmap binToNb) -- Maybe [Int]
  & fmap sum'
  where
    lineToNbUsed :: Text -> Maybe Int
    lineToNbUsed = fmap binToNb . lineToBin
    binToNb :: Text -> Int
    binToNb = T.length . T.filter (== '1')

grid :: Text -> Maybe [Text]
grid input =
  input & inputToLines & traverse lineToBin

inputToLines :: Text -> [Text]
inputToLines input = map (\i -> input <> "-" <> show i) [0..127]

lineToBin :: Text -> Maybe Text
lineToBin = hexToBin . Day10.solution2

sum' :: [Int] -> Int
sum' = foldl' (+) 0

hexToBin :: [Char] -> Maybe Text
hexToBin = fmap T.concat . traverse chexToBin

chexToBin :: Char -> Maybe Text
chexToBin '0' = Just "0000"
chexToBin '1' = Just "0001"
chexToBin '2' = Just "0010"
chexToBin '3' = Just "0011"
chexToBin '4' = Just "0100"
chexToBin '5' = Just "0101"
chexToBin '6' = Just "0110"
chexToBin '7' = Just "0111"
chexToBin '8' = Just "1000"
chexToBin '9' = Just "1001"
chexToBin 'a' = Just "1010"
chexToBin 'b' = Just "1011"
chexToBin 'c' = Just "1100"
chexToBin 'd' = Just "1101"
chexToBin 'e' = Just "1110"
chexToBin 'f' = Just "1111"
chexToBin _   = Nothing

type Coord = (Int,Int)
type Matrix = IOUArray Coord Int

bingrid :: Text -> IO (Maybe Matrix)
bingrid txt = do
  let mlines = concatMap textToArray <$> grid txt
  case mlines of
    Nothing    -> return Nothing
    Just lines -> Just <$> newListArray ((0,0),(127,127)) lines
  where
    textToArray :: Text -> [Int]
    textToArray t = T.unpack t & map (\x -> if x == '0' then -1 else 0)


solution2 :: Text -> IO Int
solution2 txt = do
  mmatrix <- bingrid txt
  case mmatrix of
    Nothing     -> return 0
    Just matrix -> fillGroups 0 matrix

fillGroups :: Int -> Matrix -> IO Int
fillGroups group matrix = do
  -- print group
  -- showMatrix matrix 16
  mc <- searchNewGroupStart matrix
  case mc of
    Nothing -> return group
    Just c  -> do
      fillGroupFrom matrix c
      numberizeGroup matrix (group + 1)
      fillGroups (group + 1) matrix

searchNewGroupStart :: Matrix -> IO (Maybe Coord)
searchNewGroupStart matrix = do
  lst <- getAssocs matrix
  return $ lst & filter ((== 0) . snd) & head & map fst

showMatrix :: Matrix -> Int -> IO ()
showMatrix matrix m =
  traverse_ (\y -> do
                vals <- traverse (\x -> readArray matrix (x,y)) [0..m]
                putText (showLine vals)) [0..m]
  where
    showLine :: [Int] -> Text
    showLine xs = T.concat lines
      where lines :: [Text]
            lines = map (\case
                            -1 -> "."
                            -2 -> "@"
                            _  -> "X") xs



fillGroupFrom :: Matrix -> Coord -> IO Matrix
fillGroupFrom matrix c@(x,y) = do
  bounds <- getBounds matrix
  writeArray matrix c (-2)
  coordsToFill <- filter isJust <$> traverse isZero (filter (inRange bounds) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)])
  traverse_ (\(Just x) -> fillGroupFrom matrix x) coordsToFill
  return matrix
  where
    isZero :: Coord -> IO (Maybe Coord)
    isZero c = do
      v <- readArray matrix c
      return $ if v == 0 then Just c else Nothing

numberizeGroup :: Matrix -> Int -> IO Matrix
numberizeGroup matrix group =
  mapArray (\x -> if x == -2 then group else x) matrix
