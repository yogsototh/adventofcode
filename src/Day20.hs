{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-|

--- Day 20: Particle Swarm ---

Suddenly, the GPU contacts you, asking for help. Someone has asked it to
simulate too many particles, and it won't be able to finish them all in time to
render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order
(starting with particle 0, then particle 1, particle 2, and so on). For each
particle, it provides the X, Y, and Z coordinates for the particle's position
(p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties are
updated in the following order:

Increase the X velocity by the X acceleration.
Increase the Y velocity by the Y acceleration.
Increase the Z velocity by the Z acceleration.
Increase the X position by the X velocity.
Increase the Y position by the Y velocity.
Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would like
to know which particle will stay closest to position <0,0,0> in the long term.
Measure this using the Manhattan distance, which in this situation is simply the
sum of the absolute values of a particle's X, Y, and Z position.

For example, suppose you are only given two particles, both of which stay
entirely on the X-axis (for simplicity). Drawing the current states of particles
0 and 1 (in that order) with an adjacent a number line and diagram of current X
positions (marked in parenthesis), the following would take place:

p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

At this point, particle 1 will never be closer to <0,0,0> than particle 0, and
so, in the long run, particle 0 will stay closest.

|-}

module Day20 where

import           Protolude

import           Control.Lens          hiding ((&))
-- import           Data.Array
import           Data.Generics.Product
-- import qualified Data.Text             as T
import qualified Data.Set              as Set
import           GHC.Generics
import           Text.Parsec           hiding (State)

type Input = [Particle]
data Particle = Particle { pos :: Coord
                         , vit :: Coord
                         , acc :: Coord
                         } deriving (Show,Eq,Generic)

data Coord = Coord { x,y,z :: Int } deriving (Show,Eq,Generic)
instance Num Coord where
  negate (Coord x y z) = Coord (negate x) (negate y) (negate z)
  (Coord x1 y1 z1) + (Coord x2 y2 z2) = Coord (x1 + x2) (y1 + y2) (z1 + z2)
  (Coord x1 y1 z1) * (Coord x2 y2 z2) = Coord (x1 * x2) (y1 * y2) (z1 * z2)
  fromInteger x = Coord (fromInteger x) 0 0
  abs (Coord x y z) = Coord (abs x) (abs y) (abs z)
  signum (Coord x y z) = Coord (signum x) (signum y) (signum z)

instance Monoid Coord where
  mempty = Coord 0 0 0

dist :: Coord -> Coord -> Int
dist c1 c2 = let dv = abs (c1 - c2) in (x dv) + (y dv) + (z dv)

parseInput :: IO Input
parseInput = parseText <$> readFile "inputs/day20.txt"

testInput :: Text
testInput = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>\n\
            \p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>\n"

parseText :: Text -> Input
parseText txt = either (const []) identity (runParser mainParse () "Input" txt)

mainParse :: Parsec Text () Input
mainParse = many1 parseParticle

parseParticle :: Parsec Text () Particle
parseParticle = Particle <$> (string "p=" *> parseCoord)
                         <*> (string ", v=" *> parseCoord)
                         <*> (string ", a=" *> parseCoord <* newline)

parseCoord :: Parsec Text () Coord
parseCoord = Coord <$> (char '<' *> parseInt)
                   <*> (char ',' *> parseInt)
                   <*> (char ',' *> parseInt <* char '>')

parseInt :: Parsec Text () Int
parseInt = do
  sgn <- optionMaybe (char '-')
  str <- many1 digit
  let strint = case sgn of
                    Nothing -> str
                    Just _  -> "-" <> str
  return $ fromMaybe 0 (reads strint & head & fmap fst)

-- Solution 1

type Solution1 = Int

data AppState = AppState { particles :: [Particle] } deriving (Show,Eq,Generic)

solution1 :: Input -> Solution1
solution1 input =
  input
  & zip ([0..] :: [Int])
  & map (\(i,p) -> (i,d (pos p), d (vit p), d (acc p)))
  & sortOn (\(_,x,_,_)->x)
  & sortOn (\(_,_,x,_)->x)
  & sortOn (\(_,_,_,x)->x)
  & head
  & fmap (^._1)
  & fromMaybe 0
  where
    d = dist mempty

-- Solution 2

type Solution2 = Int

solution2 :: Input -> Solution2
solution2 input =
  let sorted = input
               & zip ([0..] :: [Int])
               & sortOn (\(_,p)->d (pos p))
               & sortOn (\(_,p)->d (vit p))
               & sortOn (\(_,p)->d (acc p))
  in notCollided sorted & map fst & Set.fromList & Set.size
  where
    d = dist mempty
    notCollided :: [(Int,Particle)] -> [(Int,Particle)]
    notCollided sorted = do
      (i,p1) <- sorted
      (j,p2) <- sorted
      guard (j > i)
      guard (not (collide p1 p2))
      return (i,p1)
    collide :: Particle -> Particle -> Bool
    collide p1 p2 = let trivialracines = [1..10]
                    in checkRacines p1 p2 trivialracines
    potentialRacines :: Particle -> Particle -> [Int]
    potentialRacines p1 p2 =
      let a = (acc p1 ^. field @"x") - (acc p2 ^. field @"x")
          b = (vit p1 ^. field @"x") - (vit p2 ^. field @"x")
          c = (pos p1 ^. field @"x") - (pos p2 ^. field @"x")
      in
        if a /= 0
        then
          let delta = b^2 - 4*a*c in
          if delta >= 0
          then [ (-b + isqrt delta) `div` (2*a)
               , (-b - isqrt delta) `div` (2*a)
               , 1+(-b - isqrt delta) `div` (2*a)
               , 1+(-b + isqrt delta) `div` (2*a)
               , 2+(-b - isqrt delta) `div` (2*a)
               , 2+(-b + isqrt delta) `div` (2*a)
               ] & filter (>0)
          else []
        else [-c `div` b | b /= 0]
    isqrt :: Int -> Int
    isqrt = fromIntegral . floor . sqrt . fromIntegral
    pol :: Particle -> Particle -> Lens' Coord Int -> (Int -> Int)
    pol p1 p2 l = \x -> a*(x*x) + (b*x) + c
      where
        a :: Int
        a = (acc p1 ^. l) - (acc p2 ^. l)
        b :: Int
        b = (vit p1 ^. l) - (vit p2 ^. l)
        c :: Int
        c = (pos p1 ^. l) - (pos p2 ^. l)
    checkRacines :: Particle -> Particle -> [Int] -> Bool
    checkRacines _ _ [] = False
    checkRacines p1 p2 (x:xs) =
      traceShow (p1,p2,x
                , pol p1 p2 (field @"x") x == 0
                , pol p1 p2 (field @"y") x == 0
                , pol p1 p2 (field @"z") x == 0)
                (pol p1 p2 (field @"x") x == 0
                 && pol p1 p2 (field @"y") x == 0
                 && pol p1 p2 (field @"z") x == 0)
      || checkRacines p1 p2 xs


