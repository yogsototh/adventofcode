{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-| description:
--- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be
dancing.

There are sixteen programs in total, named a through p. They start by standing
in a line: a stands in position 0, b stands in position 1, and so on until p,
which stands in position 15.

The programs' dance consists of a sequence of dance moves:

- Spin, written sX, makes X programs move from the end to the front, but
  maintain their order otherwise. (For example, s3 on abcde produces cdeab).
- Exchange, written xA/B, makes the programs at positions A and B swap places.
- Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do
the following dance:

- s1, a spin of size 1: eabcd.
- x3/4, swapping the last two programs: eabdc.
- pe/b, swapping programs e and b: baedc.
- After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle
input). In what order are the programs standing after their dance?

--- Part Two ---

Now that you're starting to get a feel for the dance moves, you turn your
attention to the dance as a whole.

Keeping the positions they ended up in from their previous dance, the programs
perform it again and again: including the first dance, a total of one billion
(1000000000) times.

In the example above, their second dance would begin with the order baedc, and
use the same dance moves:

s1, a spin of size 1: cbaed.
x3/4, swapping the last two programs: cbade.
pe/b, swapping programs e and b: ceadb.

In what order are the programs standing after their billion dances?



|-}

module Day16 where

import Protolude hiding ((<|>),swap,rotate,(<>))

import Data.IORef
import Data.Semigroup (Semigroup, (<>), stimes)
import Data.Array
import Data.Array.MArray
import Data.Array.IO (IOUArray)
import Text.Parsec
import Permutations

testInput :: [Command]
testInput = readInput "s1,x3/4,pe/b"

data Command = Spin Int
             | Exchange Int Int
             | Partner Char Char
             deriving (Show)

---

parseInput :: IO [Command]
parseInput = do
  txt <- readFile "inputs/day16.txt"
  return $ readInput txt

readInput :: Text -> [Command]
readInput txt = either (const []) identity (parseTxt txt)


parseTxt :: Text -> Either ParseError [Command]
parseTxt = runParser parseCommands () "Groups 1"

parseCommands :: Parsec Text () [Command]
parseCommands = sepBy parseCommand (char ',')

parseCommand :: Parsec Text () Command
parseCommand = parseSpin <|> parseExchange <|> parsePartner

parseSpin :: Parsec Text () Command
parseSpin = Spin <$> (char 's' *> int)

parseExchange :: Parsec Text () Command
parseExchange = Exchange <$> (char 'x' *> int) <*> (char '/' *> int)

parsePartner :: Parsec Text () Command
parsePartner = Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar)

int :: Parsec Text () Int
int = do
  str <- many1 digit
  return $ fromMaybe 0 (reads str & head & fmap fst)

---

data FastArr = FastArr { arr :: IOUArray Int Char
                       , revarr :: IOUArray Char Int
                       , cursor :: IORef Int
                       , size :: Int
                       }

showFastarr :: FastArr -> IO ()
showFastarr fastarr = do
  putText "-------"
  elems <- getElems (arr fastarr)
  print elems
  getElems (revarr fastarr) >>= print
  curs <- readIORef (cursor fastarr)
  print curs
  let shift = size fastarr - curs
  print $ "=> " ++ drop shift elems ++ take shift elems


last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_:xs) = last xs

solution1 :: Int -> [Command] -> IO [Char]
solution1 len commands = do
  let letters = take len ['a'..'p']
  array <- FastArr <$> newListArray (0,len-1) letters
           <*> newListArray ('a',fromMaybe 'a' (last letters)) [0..len-1]
           <*> newIORef 0
           <*> return len
  -- showFastarr array
  traverse_ (applyCommand array) commands
  elems <- getElems (arr array)
  curs <- readIORef (cursor array)
  let shift = size array - curs
  return $ drop shift elems ++ take shift elems

applyCommand :: FastArr -> Command -> IO ()
applyCommand fastarr (Spin n) = modifyIORef (cursor fastarr) ((`mod` size fastarr) . (+n))
applyCommand fastarr (Exchange rawi rawj) = do
  cur <- readIORef (cursor fastarr)
  let s = size fastarr
      i = (rawi - cur) `mod` s
      j = (rawj - cur) `mod` s
  li <- readArray (arr fastarr) i
  lj <- readArray (arr fastarr) j
  faswap i j (arr fastarr)
  faswap li lj (revarr fastarr)
applyCommand fastarr (Partner l k) = do
  li <- readArray (revarr fastarr) l
  lj <- readArray (revarr fastarr) k
  faswap l k (revarr fastarr)
  faswap li lj (arr fastarr)

faswap :: (MArray a e m, Ix i) => i -> i -> a i e -> m ()
faswap i j arr = do
  tmpi <- readArray arr i
  tmpj <- readArray arr j
  writeArray arr i tmpj
  writeArray arr j tmpi

-- | brute force, not the real way to do it
solution2bruteforce :: Int -> [Command] -> IO [Char]
solution2bruteforce len commands = do
  let letters = take len ['a'..'p']
  array <- FastArr <$> newListArray (0,len-1) letters
           <*> newListArray ('a',fromMaybe 'a' (last letters)) [0..len-1]
           <*> newIORef 0
           <*> return len
  -- showFastarr array
  counter <- newIORef 1
  replicateM_ 1000000000 $ do
    modifyIORef counter (+1)
    i <- readIORef counter
    when (i `rem` 1000 == 0) $ print i
    traverse_ (applyCommand array) commands
  elems <- getElems (arr array)
  curs <- readIORef (cursor array)
  let shift = size array - curs
  return $ drop shift elems ++ take shift elems

data Dance = Dance { iperm :: Permutation Int
                   , cperm :: Permutation Char
                   }

applyCmd :: Command -> Dance -> Dance
applyCmd (Spin n) d@Dance{..} = d { iperm = rotate n iperm}
applyCmd (Exchange i j) d@Dance{..} = d { iperm = swap i j iperm }
applyCmd (Partner x y) d@Dance{..} = d { cperm = swap x y cperm }

solution2 :: Int -> [Command] -> [Char]
solution2 len commands = do
  let letters = take len ['a'..'p']
      lastLetter = fromMaybe 'a' (head (reverse letters))
      initDance = Dance (nullPerm (0,len-1)) (nullPerm ('a',lastLetter))
      firstDance = foldl' (flip applyCmd) initDance commands
      finalDance = stimes 1 firstDance
      tmpArray = listArray (0,len-1) (elems (unPerm (cperm finalDance)))
  permute (iperm finalDance) tmpArray & elems

instance Semigroup Dance where
  Dance r1 p1 <> Dance r2 p2 = Dance (r1 <> r2) (p1 <> p2)
