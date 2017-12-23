{-# LANGUAGE NoImplicitPrelude #-}
module Permutations where

import Protolude hiding (swap)

import           Data.Array
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeLits

newtype Permutation i =
  Permutation (Array i i)
  deriving (Eq,Ord,Show)

unPerm :: Permutation i -> Array i i
unPerm (Permutation x) = x

permute :: (Ix i,Enum i) => Permutation i -> Array i e -> Array i e
permute (Permutation p) a =
   array b [(i, a ! (p ! i)) | i <- [i0..iN] ]
  where
    b@(i0,iN) = bounds a

revpermute :: (Ix i,Enum i) => Permutation i -> Permutation i
revpermute (Permutation p) =
  Permutation $ p // [(j,i) | (i,j) <- assocs p]



testPerm :: Array Int Int
testPerm = listArray (0,4) [3,2,4,1,0]

testArray :: Array Int Char
testArray = listArray (0,4) ['a'..]

nullPerm :: (Ix i,Enum i) => (i,i) -> Permutation i
nullPerm bnds@(start,end) = Permutation $ listArray bnds [start..end]

swap :: (Ix i) => i -> i -> Permutation i -> Permutation i
swap x y (Permutation p) = Permutation (p // [(x,y), (y,x)])

rotate :: (Ix i) => Int -> Permutation i -> Permutation i
rotate n (Permutation p) =
  let is = indices p
      es = elems p
      newis = zip (drop n (cycle is)) es
  in Permutation (p // newis)

instance (Ix i,Enum i) => Semigroup (Permutation i) where
  Permutation a1 <> Permutation a2 = Permutation $
    array (bounds a2) [ (i, a2 ! j) | (i,j) <- assocs a1]

{-

0 1 2 === 0
1 0 2 => p1
0 2 1 => p2
2 1 0 => p3
1 2 0 => p4

p1 <> p2 ===> 2 0 1
p2 <> p1 ===> 1 2 0

-}

testPermutation =
  let p0 = nullPerm (0,2)
      p1 = swap 0 1 p0
      p2 = swap 1 2 p0
      p3 = p1 <> p2
  in p2 <> p1
