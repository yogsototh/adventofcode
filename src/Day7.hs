{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
description:

--- Day 7: Recursive Circus ---

Wandering further through the circuits of the computer, you come upon a tower of
programs that have gotten themselves into a bit of trouble. A recursive
algorithm has gotten out of hand, and now they're balanced precariously in a
large tower.

One program at the bottom supports the entire tower. It's holding a large disc,
and on the disc are balanced several more sub-towers. At the bottom of these
sub-towers, standing on the bottom disc, are other programs, each holding their
own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many
programs stand simply keeping the disc below them balanced but with no disc of
their own.

You offer to help, but first you need to understand the structure of these
towers. You ask each program to yell out their name, their weight, and (if
they're holding a disc) the names of the programs immediately above them
balancing on that disc. You write this information down (your puzzle input).
Unfortunately, in their panic, they don't do this in an orderly fashion; by the
time you're done, you're not sure which program gave which information.

For example, if your list is the following:

pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)

...then you would be able to recreate the structure of the towers that looks
like this:


                gyxo
              /
         ugml - ebii
       /      \
      |         jptl
      |
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         qoyq
      |
      |         ktlj
       \      /
         fwft - cntj
              \
                xhth

In this example, tknk is at the bottom of the tower (the bottom program), and is
holding up ugml, padx, and fwft. Those programs are, in turn, holding up other
programs; in this example, none of those programs are holding up any other
programs, and are all the tops of their own towers. (The actual tower balancing
in front of you is much larger.)

Before you're ready to help them, you need to make sure your information is
correct. What is the name of the bottom program?

--- Part Two ---

The programs explain the situation: they can't get down. Rather, they could get
down, if they weren't expending all of their energy trying to keep the tower
balanced. Apparently, one program has the wrong weight, and until it's fixed,
they're stuck here.

For any program holding a disc, each program standing on that disc forms a
sub-tower. Each of those sub-towers are supposed to be the same weight, or the
disc itself isn't balanced. The weight of a tower is the sum of the weights of
the programs in that tower.

In the example above, this means that for ugml's disc to be balanced, gyxo,
ebii, and jptl must all have the same weight, and they do: 61.

However, for tknk to be balanced, each of the programs standing on its disc and
all programs above it must each match. This means that the following sums must
all be the same:

ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the
other two. Even though the nodes above ugml are balanced, ugml itself is too
heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the
towers balanced. If this change were made, its weight would be 60.

Given that exactly one program is the wrong weight, what would its weight need
to be to balance the entire tower?


|-}
module Day7 where

import           Protolude

import qualified Data.Map.Strict as Map
import           Text.Parsec

data Node = Node { name   :: Text
                 , weight :: Int
                 , sons   :: [Text]
                 } deriving (Eq, Ord,Show)
type Nodes = Map Text Node

parseInput :: IO Nodes
parseInput = do
  str <- readFile "inputs/day7.txt"
  return $ either (const Map.empty) identity (parseNodes str)

parseNodes :: Text -> Either ParseError Nodes
parseNodes = parse nodes "Nodes"

nodes :: Parsec Text () Nodes
nodes = many1 parseNode
  & fmap (Map.fromList . map (\n -> (name n,n)))

int :: Parsec Text () Int
int = do
  str <- many1 digit
  return $ fromMaybe 0 (reads str & head & fmap fst)

sonsP :: Parsec Text () [Text]
sonsP = do
  c <- anyChar
  if c == '\n'
    then return []
    else do
      string "-> "
      fmap toS (many1 letter) `sepBy` string ", " <* char '\n'

parseNode :: Parsec Text () Node
parseNode =
  Node <$> fmap toS (many1 letter) <* char ' '
       <*> (char '(' *> int <* char ')')
       <*> sonsP

testInput :: Text
testInput =  "pbga (66)\n\
             \xhth (57)\n\
             \ebii (61)\n\
             \havc (66)\n\
             \ktlj (57)\n\
             \fwft (72) -> ktlj, cntj, xhth\n\
             \qoyq (66)\n\
             \padx (45) -> pbga, havc, qoyq\n\
             \tknk (41) -> ugml, padx, fwft\n\
             \jptl (61)\n\
             \ugml (68) -> gyxo, ebii, jptl\n\
             \gyxo (61)\n\
             \cntj (57)\n"

testNodes :: Nodes
testNodes = either (const Map.empty) identity (parseNodes testInput)

fatherOf :: Nodes -> Text -> Maybe Node
fatherOf nodes nodeName = Map.elems nodes
  & filter (\n -> nodeName `elem` sons n)
  & head

rootOf :: Nodes -> Maybe Node
rootOf nodes = go nodes (head (Map.elems nodes))
  where
    go :: Nodes -> Maybe Node -> Maybe Node
    go nodes (Just node) = case fatherOf nodes (name node) of
                             Just n  -> go nodes (Just n)
                             Nothing -> Just node
    go _ _ = Nothing

data Tree a = TNode a [Tree a] deriving (Eq,Ord,Show)

totalWeight :: Tree (a,Int) -> Int
totalWeight (TNode (_,i) _) = i

weightedNodes :: Nodes -> Maybe (Tree (Node,Int))
weightedNodes nodes = fmap go (rootOf nodes)
  where
    go :: Node -> Tree (Node,Int)
    go node =
      let subtrees :: [Tree (Node,Int)]
          subtrees = sons node
                     & map (`Map.lookup` nodes)
                     & sequenceA
                     & maybe [] (map go)
          sonweights = map totalWeight subtrees
      in TNode (node,foldl' (+) (weight node) sonweights)
                       subtrees

histogram :: Ord a => [a] -> Map.Map a Int
histogram xs = Map.fromListWith (+) $ zip xs (repeat 1)

solution2 :: Nodes -> Maybe (Text,Int)
solution2 nodes = do
  tree <- weightedNodes nodes
  go 0 tree
  where
    go :: Int -> Tree (Node,Int) -> Maybe (Text,Int)
    go expectedWeight (TNode (n,nw) subtrees) =
       let groupedSubWeight = histogram (map totalWeight subtrees)
       in if Map.size groupedSubWeight /= 1
             then do
                  expW <- groupedSubWeight
                            & Map.filter (/= 1)
                            & Map.keys
                            & head
                  badSubtree <- filter ((/= expW) . totalWeight) subtrees & head
                  go expW badSubtree
             else return (name n, weight n - (nw - expectedWeight))
