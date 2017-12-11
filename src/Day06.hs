{-# LANGUAGE NoImplicitPrelude #-}
{-|
description:
--- Day 6: Memory Reallocation ---

A debugger program here is having an issue: it is trying to repair a memory
reallocation routine, but it keeps getting stuck in an infinite loop.

In this area, there are sixteen memory banks; each memory bank can hold any
number of blocks. The goal of the reallocation routine is to balance the blocks
between the memory banks.

The reallocation routine operates in cycles. In each cycle, it finds the memory
bank with the most blocks (ties won by the lowest-numbered memory bank) and
redistributes those blocks among the banks. To do this, it removes all of the
blocks from the selected bank, then moves to the next (by index) memory bank and
inserts one of the blocks. It continues doing this until it runs out of blocks;
if it reaches the last memory bank, it wraps around to the first one.

The debugger would like to know how many redistributions can be done before a
blocks-in-banks configuration is produced that has been seen before.

For example, imagine a scenario with only four memory banks:

- The banks start with 0, 2, 7, and 0 blocks.
  The third bank has the most blocks, so it is chosen for redistribution.
- Starting with the next bank (the fourth bank) and then continuing to the
  first bank, the second bank, and so on, the 7 blocks are spread out over the
  memory banks. The fourth, first, and second banks get two blocks each, and
  the third bank gets one back. The final result looks like this: 2 4 1 2.
- Next, the second bank is chosen because it contains the most blocks (four).
  Because there are four memory banks, each gets one block.
  The result is: 3 1 2 3.
- Now, there is a tie between the first and fourth memory banks, both of which
  have three blocks. The first bank wins the tie, and its three blocks are
  distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
- The fourth bank is chosen, and its four blocks are distributed such that
  each of the four banks receives one: 1 3 4 1.
- The third bank is chosen, and the same thing happens: 2 4 1 2.
- At this point, we've reached a state we've seen before: 2 4 1 2
  was already seen.
  The infinite loop is detected after the fifth block redistribution cycle,
  and so the answer in this example is 5.

Given the initial block counts in your puzzle input,
how many redistribution cycles must be completed before a configuration is
produced that has been seen before?

--- Part Two ---

Out of curiosity, the debugger would also like to know the size of the loop:
starting from a state that has already been seen, how many block redistribution
cycles must be performed before that same state is seen again?

In the example above, 2 4 1 2 is seen again after four cycles, and so the answer
in that example would be 4.

How many cycles are in the infinite loop that arises from the configuration in
your puzzle input?

|-}
module Day06 where

import           Protolude

import           Data.Array.IO (IOUArray)
import qualified Data.Array.IO as Array
import qualified Data.Set      as Set
import qualified Data.Text     as Text

type Arr = IOUArray Int Int

parseInput :: IO Arr
parseInput = do
  inputTxt <- readFile "inputs/day6.txt"
  let lst = inputTxt & Text.words
            & traverse (strToInteger . toS)
            & fromMaybe []
      nb = length lst
  Array.newListArray (0,nb-1) lst

strToInteger :: [Char] -> Maybe Int
strToInteger = fmap fst . head . reads

nextBank :: Arr -> Int -> IO Int
nextBank arr bank = do
  (start,end) <- Array.getBounds arr
  return $ if bank == end then start else bank + 1

oneSpread :: (Int,Int) -> Arr -> Int -> Int -> IO Arr
oneSpread (start,end) arr currentBank nbBlocks =
  if nbBlocks == 0 then
    return arr
  else do
    v <- Array.readArray arr currentBank
    Array.writeArray arr currentBank (v+1)
    next <- nextBank arr currentBank
    oneSpread
      (start,end)
      arr
      next
      (nbBlocks - 1)

oneCycle :: Set [Int] -> Arr -> IO (Set [Int],Bool)
oneCycle memory arr = do
  bank <- findArgMaxBank arr
  next <- nextBank arr bank
  nbBlocks <- Array.readArray arr bank
  Array.writeArray arr bank 0
  bounds <- Array.getBounds arr
  newArr <- oneSpread bounds arr next nbBlocks
  elems <- Array.getElems newArr
  let nextMemory = Set.insert elems memory
  -- print elems
  return (nextMemory,Set.member elems memory)

findArgMaxBank :: Arr -> IO Int
findArgMaxBank arr = do
  lst <- Array.getAssocs arr
  return $ lst
    & sortBy (\x y -> compare (snd y) (snd x))
    & head
    & fmap fst
    & fromMaybe 0

testArray :: IO Arr
testArray = Array.newListArray (0,3) [0,2,7,0]

solution1 input =
  go input Set.empty 0
  where
    go :: Arr -> Set [Int] -> Int -> IO Int
    go arr memory n = do
      (newMemory,isRepeated) <- oneCycle memory arr
      if isRepeated
        then return (n+1)
        else go arr newMemory (n+1)


oneCycle2 :: [Int] -> Arr -> IO Bool
oneCycle2 match arr = do
  bank  <- findArgMaxBank arr
  next  <- nextBank arr bank
  nbBlocks <- Array.readArray arr bank
  Array.writeArray arr bank 0
  bounds <- Array.getBounds arr
  newArr <- oneSpread bounds arr next nbBlocks
  elems  <- Array.getElems newArr
  return (elems == match)

solution2 input =
  go input Set.empty 0
  where
    go :: Arr -> Set [Int] -> Int -> IO Int
    go arr memory n = do
      (newMemory,isRepeated) <- oneCycle memory arr
      if isRepeated
        then do
          elems <- Array.getElems arr
          go2 elems arr 0
        else go arr newMemory (n+1)
    go2 :: [Int] -> Arr -> Int -> IO Int
    go2 match arr n = do
      isRepeated <- oneCycle2 match arr
      if isRepeated
        then return (n+1)
        else go2 match arr (n+1)
