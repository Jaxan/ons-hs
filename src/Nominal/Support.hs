{-# LANGUAGE DerivingVia #-}

module Nominal.Support where

import qualified Data.List as List
import qualified Data.List.Ordered as OrdList

import Nominal.Atom

-- * Support

-- | A support is a finite set of 'Atom's. In the implementation, it is
-- represented by a sorted list.
newtype Support = Support {unSupport :: [Atom]}
  deriving (Eq, Ord)
  deriving Show via [Atom]

-- ** Queries

size :: Support -> Int
size = List.length . unSupport

null :: Support -> Bool
null = List.null . unSupport

min :: Support -> Atom
min = List.head . unSupport

-- ** Construction

empty :: Support
empty = Support []

singleton :: Atom -> Support
singleton r = Support [r]

-- | Returns a "default" support with n elements.
def :: Int -> Support
def n = fromDistinctAscList . fmap (Atom . fromIntegral) $ [1 .. n]

-- ** Set operations

union :: Support -> Support -> Support
union (Support x) (Support y) = Support (OrdList.union x y)

intersect :: Support -> Support -> Support
intersect (Support x) (Support y) = Support (OrdList.isect x y)

toList :: Support -> [Atom]
toList = unSupport

-- ** Conversion to/from lists

fromList, fromAscList, fromDistinctAscList :: [Atom] -> Support
fromList = Support . OrdList.nubSort
fromAscList = Support . OrdList.nub
fromDistinctAscList = Support

{- NOTE: I have tried using a `Data.Set` data structure for supports, but it
was slower. Using lists is fast enough, perhaps other data structures like
vectors could be considered at some point.
-}
