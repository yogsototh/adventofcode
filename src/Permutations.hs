{-# LANGUAGE NoImplicitPrelude #-}
module Permutations where

import Protolude hiding (swap)

import           Data.Array
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import           GHC.TypeLits

-- | properties
-- a permutation is a bijection
newtype Permutation i =
  Permutation { unPerm :: Array i i }
  deriving (Eq,Ord,Show)

isBijection :: (Ix i) => Permutation i -> Bool
isBijection p =
  Set.fromList (elems (unPerm p)) == Set.fromList (indices (unPerm p))

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
    array (bounds a1) [ (i, a1 ! j) | (i,j) <- assocs a2]
