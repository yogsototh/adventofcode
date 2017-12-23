{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|

Somehow, a network packet got lost and ended up here. It's trying to follow a
routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -,
and +) show the path it needs to take, starting by going down onto the only line
connected to the top of the diagram. It needs to follow this path until it
reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to continue
going the same direction, and only turn left or right when there's no other
option. In addition, someone has left letters on the line; these also don't
change its direction, but it can use them to keep track of where it's been. For
example:


     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+

Given this diagram, the packet needs to take the following path:

- Starting at the only line touching the top of the diagram, it must go down,
  pass through A, and continue onward to the first +.
- Travel right, up, and right, passing through B in the process.
- Continue down (collecting C), right, and up (collecting D).
- Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What
letters will it see (in the order it would see them) if it follows the path?
(The routing diagram is very wide; make sure you view it without line wrapping.)

--- Part Two ---

The packet is curious how many steps it needs to go.

For example, using the same routing diagram from the example above...

     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 

...the packet would go:

- 6 steps down (including the first line at the top of the diagram).
- 3 steps right.
- 4 steps up.
- 3 steps right.
- 4 steps down.
- 3 steps right.
- 2 steps up.
- 13 steps left (including the F it stops on).

This would result in a total of 38 steps.

How many steps does the packet need to go?

|-}

module Day19 where

import           Protolude

import           Control.Lens          hiding ((&))
import           Data.Array
import           Data.Generics.Product hiding (position)
import qualified Data.Text             as T
import           GHC.Generics

type Grid = Array (Int,Int) Char

parseInput :: IO Grid
parseInput = parseTxt <$> readFile "inputs/day19.txt"

testInput :: Text
testInput = "     |          \n\
            \     |  +--+    \n\
            \     A  |  C    \n\
            \ F---|----E|--+ \n\
            \     |  |  |  D \n\
            \     +B-+  +--+ \n"

parseTxt :: Text -> Grid
parseTxt txt =
  let
    sentences :: [[Char]]
    sentences = txt & T.lines & map T.unpack
    bounds = ((1,1),(length sentences, maybe 0 length (head sentences)))
  in listArray bounds (concat sentences)

data AppState =
  AppState { letters   :: [Char]
           , position  :: (Int,Int)
           , direction :: (Int,Int)
           , nbSteps   :: Int
           , grid      :: Grid
           } deriving (Eq, Show, Generic)

solution1 :: Grid -> [Char]
solution1 grid =
  let finalState = execState runPath (AppState [] (1,1) (1,0) 0 grid)
  in letters finalState & reverse

runPath :: State AppState ()
runPath = do
  searchTop
  followPath

searchTop :: State AppState ()
searchTop = do
  grid <- use (field @"grid")
  pos <- use (field @"position")
  -- debugtr "searchTop"
  when (grid ! pos /= '|') $ do
      field @"position" . _2 += 1
      searchTop

debugtr :: Text -> State AppState ()
debugtr str = do
  grid <- use (field @"grid")
  pos <- use (field @"position")
  dir <- use (field @"direction")
  ls <- use (field @"letters")
  let res = (str,pos,grid ! pos,dir,reverse ls)
  traceShow (deepseq res res) return ()

followPath :: State AppState ()
followPath = do
  grid <- use (field @"grid")
  pos <- use (field @"position")
  -- debugtr "followPath"
  case grid ! pos of
    '+' -> searchNewDirection
    ' ' -> continue
    '|' -> continue
    '-' -> continue
    c -> do
      field @"letters" %= (c:)
      searchNewDirection

addDir :: (Int,Int) -> (Int,Int) -> (Int,Int)
addDir (x1,y1) (x2,y2) = (x1+x2,y1+y2)

continue :: State AppState ()
continue = do
  dir <- use (field @"direction")
  field @"position" %= addDir dir
  field @"nbSteps" += 1
  -- debugtr "continue"
  followPath

inBound :: ((Int,Int),(Int,Int)) -> (Int,Int) -> Bool
inBound ((minx,miny),(maxx,maxy)) (x,y) =
  minx <= x && x <= maxx && miny <= y && y <= maxy

backdir :: (Int,Int) -> (Int,Int)
backdir (x,y) = (-x,-y)

searchNewDirection :: State AppState ()
searchNewDirection = do
  grid <- use (field @"grid")
  (posx,posy) <- use (field @"position")
  dir <- use (field @"direction")
  let
    bnds = bounds grid
    possibleDirs = [(1,0), (0,-1), (-1,0), (0,1)]
                 & filter (/= backdir dir)
                 & map (\(dx,dy)->((dx,dy),(posx+dx,posy+dy)))
                 & filter (\(_,p)-> inBound bnds p && grid ! p /= ' ')
  -- traceShow possibleDirs $ return ()
  case possibleDirs of
    [(newdir,_)] -> do
      field @"direction" .= newdir
      continue
    _ -> return ()

solution2 :: Grid -> Int
solution2 grid =
  let finalState = execState runPath (AppState [] (1,1) (1,0) 0 grid)
  in 1 + nbSteps finalState
