{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-|
--- Day 18: Duet ---

You discover a tablet containing some strange assembly code labeled simply
"Duet". Rather than bother the sound card with it, you decide to run the code
yourself. Unfortunately, you don't see any documentation, so you're left to
figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are
each named with a single letter and that can each hold a single integer. You
suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what
they do. Here's what you determine:

- snd X plays a sound with a frequency equal to the value of X.
- set X Y sets register X to the value of Y.
- add X Y increases register X by the value of Y.
- mul X Y sets register X to the result of multiplying the value contained in
  register X by the value of Y.
- mod X Y sets register X to the remainder of dividing the value contained in
  register X by the value of Y (that is, it sets X to the result of X modulo Y).
- rcv X recovers the frequency of the last sound played, but only when the value
  of X is not zero. (If it is zero, the command does nothing.)
- jgz X Y jumps with an offset of the value of Y, but only if the value of X is
  greater than zero. (An offset of 2 skips the next instruction, an offset of -1
  jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a
number. The value of a register is the integer it contains; the value of a
number is that number.

After each jump instruction, the program continues with the instruction to which
the jump jumped. After any other instruction, the program continues with the
next instruction. Continuing (or jumping) off either end of the program
terminates it.

For example:

set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2

- The first four instructions set a to 1, add 2 to it, square it, and then set
  it to itself modulo 5, resulting in a value of 4.
- Then, a sound with frequency 4 (the value of a) is played.
- After that, a is set to 0, causing the subsequent rcv and jgz instructions to
  both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
- Finally, a is set to 1, causing the next jgz instruction to activate, jumping
  back two instructions to another jump, which jumps again to the rcv, which
  ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last sound
played is 4.

What is the value of the recovered frequency (the value of the most recently
played sound) the first time a rcv instruction is executed with a non-zero
value?

--- Part Two ---

As you congratulate yourself for a job well done, you notice that the
documentation has been on the back of the tablet this entire time. While you
actually got most of the instructions correct, there are a few key differences.
This assembly code isn't about sound at all - it's meant to be run twice at the
same time.

Each running copy of the program has its own set of registers and follows the
code independently - in fact, the programs don't even necessarily run at the
same speed. To coordinate, they use the send (snd) and receive (rcv)
instructions:

- snd X sends the value of X to the other program. These values wait in a queue
  until that program is ready to receive them. Each program has its own message
  queue, so a program can never receive a message it sent.

- rcv X receives the next value and stores it in register X. If no values are in
  the queue, the program waits for a value to be sent to it. Programs do not
  continue to the next instruction until they have received a value. Values are
  received in the order they are sent.

Each program also has its own program ID (one 0 and the other 1); the register p
should begin with this value.

For example:

snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d

Both programs begin by sending three values to the other. Program 0 sends 1, 2,
0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and
stores it in a, receives another value (both 2) and stores it in b, and then
each receives the program ID of the other program (program 0 receives 1; program
1 receives 0) and stores it in c. Each program now sees a different value in its
own copy of register c.

Finally, both programs try to rcv a fourth time, but no data is waiting for
either of them, and they reach a deadlock. When this happens, both programs
terminate.

It should be noted that it would be equally valid for the programs to run at
different speeds; for example, program 0 might have sent all three values and
then stopped at the first rcv before program 1 executed even its first
instruction.

Once both of your programs have terminated (regardless of what caused them to do
so), how many times did program 1 send a value?

|-}

module Day18 where

import           Data.Array
import qualified Data.Char  as C
import qualified Data.Map   as Map
import qualified Data.Text  as T
import           Protolude

type Program = Array Int Instruction

newtype Reg = Reg Text deriving (Show,Eq,Ord)

data Value = R Reg | I Int deriving (Show)

data Instruction =
  Snd Value
  | Set Reg Value
  | Add Reg Value
  | Mul Reg Value
  | Mod Reg Value
  | Rcv Reg
  | Jgz Value Value
  deriving (Show)

parseInput :: IO Program
parseInput = parseTxt <$> readFile "inputs/day18.txt"

parseTxt :: Text -> Program
parseTxt txt = let instr = txt & T.lines & map (parseInstr . T.words) in
  listArray (0,length instr-1) instr

txtToInt :: Text -> Maybe Int
txtToInt = fmap fst . head . reads . toS

parseValue :: Text -> Value
parseValue t = let c = T.head t in
  if C.isLetter c then R (Reg t) else I (fromMaybe 0 (txtToInt t))

parseInstr :: [Text] -> Instruction
parseInstr ["snd",v]     = Snd (parseValue v)
parseInstr ["set",r,v]   = Set (Reg r) (parseValue v)
parseInstr ["add",r,v]   = Add (Reg r) (parseValue v)
parseInstr ["mul",r,v]   = Mul (Reg r) (parseValue v)
parseInstr ["mod",r,v]   = Mod (Reg r) (parseValue v)
parseInstr ["rcv",r]     = Rcv (Reg r)
parseInstr ["jgz",v1,v2] = Jgz (parseValue v1) (parseValue v2)
parseInstr _             = error "Don't knwow this instruction"

testInput :: Text
testInput = "set a 1\n\
            \add a 2\n\
            \mul a a\n\
            \mod a 5\n\
            \snd a\n\
            \set a 0\n\
            \rcv a\n\
            \jgz a -1\n\
            \set a 1\n\
            \jgz a -2\n"

data ProgState = ProgState { mem        :: Map.Map Reg Int
                           , cursor     :: Int
                           , lastPlayed :: Maybe Int
                           , lastRcv    :: Maybe Int
                           } deriving (Show)

solution1 :: Program -> Maybe Int
solution1 p = go p initState
  where
    initState = ProgState mempty 0 Nothing Nothing
    go :: Program -> ProgState -> Maybe Int
    go p st@ProgState{..} =
      -- traceShow st $
      case lastRcv of
        Just x -> Just x
        Nothing -> let (start,stop) = bounds p in
          if cursor < start || cursor > stop
          then Nothing
          else go p (nextInstr (p!cursor) st)

nextInstr :: Instruction -> ProgState -> ProgState
nextInstr (Snd v)   st@ProgState{..} =
  st { lastPlayed = toValue v st
     , cursor = cursor + 1}
nextInstr (Set r v) st@ProgState{..} =
  st { mem = Map.insert r (fromMaybe 0 (toValue v st)) mem
     , cursor = cursor + 1}

nextInstr (Add r v) st@ProgState{..} =
  st { mem = Map.insert r (x+y) mem
     , cursor = cursor + 1}
  where
    x = fromMaybe 0 (toValue (R r) st)
    y = fromMaybe 0 (toValue v st)

nextInstr (Mul r v) st@ProgState{..} =
  st { mem = Map.insert r (x*y) mem
     , cursor = cursor + 1}
  where
    x = fromMaybe 0 (toValue (R r) st)
    y = fromMaybe 0 (toValue v st)

nextInstr (Mod r v) st@ProgState{..} =
  st { mem = Map.insert r (x `mod` y) mem
     , cursor = cursor + 1}
  where
    x = fromMaybe 0 (toValue (R r) st)
    y = fromMaybe 0 (toValue v st)

nextInstr (Rcv v) st@ProgState{..} =
  st { lastRcv = case toValue (R v) st of
                   Just 0 -> lastRcv
                   Nothing -> lastRcv
                   _ -> lastPlayed
     , cursor = cursor + 1}

nextInstr instr@(Jgz v1 v2) st@ProgState{..} =
  st { cursor = cursor + if x1 > 0 then x2 else 1 }
  where
    x1 = fromMaybe 0 (toValue v1 st)
    x2 = fromMaybe 0 (toValue v2 st)

toValue :: Value -> ProgState -> Maybe Int
toValue (I i) _             = Just i
toValue (R c) ProgState{..} = Map.lookup c mem


-- Sol 2


data P2State =
  P2State { mem :: Map.Map Reg Int
          , cur :: Int
          , buffer :: [Int]
          , nbSend :: Int
          , waiting :: Bool
          } deriving (Show)

type S2State = (P2State,P2State)
solution2 :: Program -> Int
solution2 p = go p initState
  where
    initState :: S2State
    initState = ( P2State (Map.fromList [(Reg "p",0)]) 0 [] 0 False
                , P2State (Map.fromList [(Reg "p",1)]) 0 [] 0 False)
    terminated p P2State{..} =
      let (start,stop) = bounds p in cur < start || cur > stop
    go :: Program -> S2State -> Int
    go p (st0,st1) =
      if ((terminated p st0) || (waiting st0))
         && ((terminated p st1) || (waiting st1))
      then nbSend st1
      else
        let
        (nextst0,msgs1) = if terminated p st0
                          then (st0,[])
                          else n2Instr (p ! cur st0) st0
        (nextst1,msgs0) = if terminated p st1
                          then (st1,[])
                          else n2Instr (p ! cur st1) st1
        in
        go p ( nextst0 { buffer = buffer nextst0 ++ msgs0 }
             , nextst1 { buffer = buffer nextst1 ++ msgs1 })

m2list :: Maybe a -> [a]
m2list Nothing = []
m2list (Just x) = [x]

n2Instr :: Instruction -> P2State -> (P2State,[Int])
n2Instr (Snd v)   st@P2State{..} =
  (st {cur = cur + 1
      , nbSend = nbSend + 1}, m2list (toV v st))

n2Instr (Set r v) st@P2State{..} =
  (st { mem = Map.insert r (fromMaybe 0 (toV v st)) mem
     , cur = cur + 1}, [])

n2Instr (Add r v) st@P2State{..} =
  (st { mem = Map.insert r (x+y) mem
     , cur = cur + 1}
  , [])
  where
    x = fromMaybe 0 (toV (R r) st)
    y = fromMaybe 0 (toV v st)

n2Instr (Mul r v) st@P2State{..} =
  (st { mem = Map.insert r (x*y) mem
     , cur = cur + 1}
  , [])
  where
    x = fromMaybe 0 (toV (R r) st)
    y = fromMaybe 0 (toV v st)

n2Instr (Mod r v) st@P2State{..} =
  (st { mem = Map.insert r (x `mod` y) mem
     , cur = cur + 1}
  , [])
  where
    x = fromMaybe 0 (toV (R r) st)
    y = fromMaybe 0 (toV v st)

n2Instr (Rcv r) st@P2State{..} =
  if null buffer
  then (st {waiting = True}, [])
  else let (x:xs) = buffer in
    (st { buffer = xs
        , mem = Map.insert r x mem
        , cur = cur + 1
        }
    , [])

n2Instr instr@(Jgz v1 v2) st@P2State{..} =
  (st { cur = cur + if x1 > 0 then x2 else 1 },[])
  where
    x1 = fromMaybe 0 (toV v1 st)
    x2 = fromMaybe 0 (toV v2 st)

toV :: Value -> P2State -> Maybe Int
toV (I i) _             = Just i
toV (R c) P2State{..} = Map.lookup c mem
