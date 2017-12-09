{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
description:
--- Day 8: I Heard You Like Registers ---

You receive a signal directly from the CPU. Because of your recent assistance
with jump instructions, it would like you to compute the result of a series of
unusual register instructions.

Each instruction consists of several parts: the register to modify, whether to
increase or decrease that register's value, the amount by which to increase or
decrease it, and a condition. If the condition fails, skip the instruction
without modifying the register. The registers all start at 0. The instructions
look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10

These instructions would be processed as follows:

- Because a starts at 0, it is not greater than 1, and so b is not modified.
- a is increased by 1 (to 1) because b is less than 5 (it is 0).
- c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
- c is increased by -20 (to -10) because c is equal to 10.
- After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to).
However, the CPU doesn't have the bandwidth to tell you what all the registers
are named, and leaves that to you to determine.

What is the largest value in any register after completing the instructions in
your puzzle input?

|-}
module Day8 where

import           Protolude hiding ((<|>),many)

import qualified Data.Map.Strict as Map
import           Text.Parsec

testInput :: Text
testInput = "b inc 5 if a > 1\n\
            \a inc 1 if b < 5\n\
            \c dec -10 if a >= 1\n\
            \c inc -20 if c == 10\n"

type Registers = Map Text Int

data Op = Inc | Dec deriving (Show)
data TestOp = Sup | SupEq | Less | LessEq | Equal | Different deriving (Show)
data Condition = Condition { regName :: Text
                           , testOp :: TestOp
                           , val :: Int
                           } deriving (Show)
data Instruction = Instruction { registerName :: Text
                               , op :: Op
                               , nb :: Int
                               , cond :: Condition
                               } deriving (Show)
type Instructions = [Instruction]

parseInput :: IO Instructions
parseInput = do
  str <- readFile "inputs/day8.txt"
  return $ either (const []) identity (parseInstructions str)

parseInstructions :: Text -> Either ParseError Instructions
parseInstructions = parse instructions "Instructions"

instructions :: Parsec Text () Instructions
instructions = many1 parseInstruction

parseInstruction :: Parsec Text () Instruction
parseInstruction =
  Instruction <$> regname <* char ' '
              <*> parseOp <* char ' '
              <*> int <* char ' '
              <*> parseCond <* char '\n'

regname :: Parsec Text () Text
regname = fmap toS (many1 letter)

int :: Parsec Text () Int
int = do
  c <- char '-' <|> digit
  str <- many digit
  return $ fromMaybe 0 (reads (c:str) & head & fmap fst)

parseOp :: Parsec Text () Op
parseOp = do
  instr <- many1 letter
  case instr of
    "inc" -> return Inc
    "dec" -> return Dec
    _ -> unexpected "should be inc or dec"

parseCond :: Parsec Text () Condition
parseCond = do
  string "if "
  Condition <$> fmap toS (many1 letter) <* char ' '
            <*> parseTestOp
            <*> int

parseTestOp :: Parsec Text () TestOp
parseTestOp = do
  c1 <- char '<' <|> char '=' <|> char '>' <|> char '!'
  case c1 of
    '<' -> (char ' ' >> return Less) <|> (char '=' >> char ' ' >> return LessEq)
    '>' -> (char ' ' >> return Sup) <|> (char '=' >> char ' ' >> return SupEq)
    '=' -> char '=' >> char ' ' >> return Equal
    '!' -> char '=' >> char ' ' >> return Different
    _ -> unexpected "Should be <, >, <=, >=, != or =="

testInstructions :: Instructions
testInstructions = either (const []) identity (parseInstructions testInput)

evalInstructions :: Instruction -> Registers -> Registers
evalInstructions (Instruction regname
                             instrOp
                             instrNb
                             (Condition condReg condTest condVal)) st = do
  let v = fromMaybe 0 (Map.lookup condReg st)
      opfn = case condTest of
               Less -> (<)
               LessEq -> (<=)
               Sup -> (>)
               SupEq -> (>=)
               Equal -> (==)
               Different -> (/=)
      condMet = v `opfn` condVal
      action = case instrOp of
                 Dec -> (-)
                 Inc -> (+)
      st2 = maybe (Map.insert regname 0 st) (const st) (Map.lookup regname st)
  if condMet
    then Map.adjust (`action` instrNb) regname st2
    else st2

solution1 :: Instructions -> Int
solution1 instructions = go instructions Map.empty
  where
     go :: Instructions -> Registers -> Int
     go [] reg = maximum (Map.elems reg)
     go (instr:is) reg = go is (evalInstructions instr reg)

solution2 :: Instructions -> Int
solution2 instructions = go instructions Map.empty 0
  where
     go :: Instructions -> Registers -> Int -> Int
     go [] reg m = maximum (m:Map.elems reg)
     go (instr:is) reg m =
       go is (evalInstructions instr reg) (maximum (m:Map.elems reg))

