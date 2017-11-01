{-# LANGUAGE DeriveGeneric #-}

module Support where

import qualified Data.List as List
import qualified Data.List.Ordered as OrdList
import GHC.Generics (Generic)


-- We take some model of the dense linear order. The rationals are a natural
-- choice. (Note that every countable model is order-isomorphic, so it doesn't
-- matter so much in the end.) I wrap it in a newtype, so we will only use the
-- Ord instances, and because it's not very nice to work with type synonyms.
-- Show instance included for debugging.
newtype Rat = Rat { unRat :: Rational }
  deriving (Eq, Ord, Show, Generic)

-- A support is a set of rational numbers, which can always be ordered. I tried
-- an implementation using Data.Set, it was slower. We could also use Vectors?
-- Note that a sorted list makes sense in many cases, since we do not really
-- need membership queries on this type. Maybe make this into a newtype.
type Support = [Rat] -- always sorted

size :: Support -> Int
size = List.length

null :: Support -> Bool
null = List.null

min :: Support -> Rat
min = List.head

empty :: Support
empty = []

union :: Support -> Support -> Support
union = OrdList.union

singleton :: Rat -> Support
singleton r = [r]

toList :: Support -> Support
toList = id

fromList, fromAscList, fromDistinctAscList :: [Rat] -> Support
fromList = OrdList.nubSort
fromAscList = OrdList.nub
fromDistinctAscList = id

def :: Int -> Support
def n = fromDistinctAscList . fmap (Rat . toRational) $ [1..n]

{-
-- The Data.Set implementation
import Data.Set (Set)
import qualified Data.Set as Set

type Support = Set Rat

size :: Support -> Int
size = Set.size

null :: Support -> Bool
null = Set.null

min :: Support -> Rat
min = Set.findMin

empty :: Support
empty = Set.empty

union :: Support -> Support -> Support
union = Set.union

singleton :: Rat -> Support
singleton = Set.singleton

toList :: Support -> [Rat]
toList = Set.toAscList

fromList, fromAscList, fromDistinctAscList :: [Rat] -> Support
fromList = Set.fromList
fromAscList = Set.fromAscList
fromDistinctAscList = Set.fromDistinctAscList
-}
