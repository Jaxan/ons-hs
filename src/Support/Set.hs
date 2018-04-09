module Support.Set where

import Data.Set (Set)
import qualified Data.Set as Set

import Support.Rat

-- Tree-based ordered set
newtype Support = Support { unSupport :: Set Rat }

size :: Support -> Int
size = Set.size . unSupport

null :: Support -> Bool
null = Set.null . unSupport

min :: Support -> Rat
min = Set.findMin . unSupport

empty :: Support
empty = Support Set.empty

union :: Support -> Support -> Support
union (Support x) (Support y) = Support (Set.union x y)

singleton :: Rat -> Support
singleton = Support . Set.singleton

toList :: Support -> [Rat]
toList = Set.toAscList . unSupport

fromList, fromAscList, fromDistinctAscList :: [Rat] -> Support
fromList = Support . Set.fromList
fromAscList = Support . Set.fromAscList
fromDistinctAscList = Support . Set.fromDistinctAscList


