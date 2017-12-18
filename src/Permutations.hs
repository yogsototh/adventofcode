module Permutations where

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

testPerm :: Array Int Int
testPerm = listArray (0,4) [3,2,4,1,0]

testArray :: Array Int Char
testArray = listArray (0,4) ['a'..]

nullPerm :: (Ix i,Enum i) => (i,i) -> Permutation i
nullPerm bnds@(start,end) = Permutation $ listArray bnds [start..end]

swap :: (Ix i) => i -> i -> Permutation i -> Permutation i
swap x y (Permutation p) = Permutation (p // [(x,p ! y), (y,p ! x)])

rotate :: (Ix i) => Int -> Permutation i -> Permutation i
rotate n (Permutation p) =
  let is = indices p
      es = elems p
      newis = zip (drop n (cycle is)) es
  in Permutation (p // newis)

instance (Ix i,Enum i) => Semigroup (Permutation i) where
  p1 <> Permutation a2 = Permutation $ permute p1 a2
