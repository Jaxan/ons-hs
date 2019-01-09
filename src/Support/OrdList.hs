module Support.OrdList where

import qualified Data.List as List
import qualified Data.List.Ordered as OrdList

import Support.Rat

-- always sorted
newtype Support = Support { unSupport :: [Rat] }
  deriving (Eq, Ord)

instance Show Support where
  show = show . unSupport

size :: Support -> Int
size = List.length . unSupport

null :: Support -> Bool
null = List.null . unSupport

min :: Support -> Rat
min = List.head . unSupport

empty :: Support
empty = Support []

union :: Support -> Support -> Support
union (Support x) (Support y) = Support (OrdList.union x y)

intersect :: Support -> Support -> Support
intersect (Support x) (Support y) = Support (OrdList.isect x y)

singleton :: Rat -> Support
singleton r = Support [r]

toList :: Support -> [Rat]
toList = unSupport

fromList, fromAscList, fromDistinctAscList :: [Rat] -> Support
fromList = Support . OrdList.nubSort
fromAscList = Support . OrdList.nub
fromDistinctAscList = Support
