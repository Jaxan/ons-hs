{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EquivariantMap where

import Data.Semigroup (Semigroup)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import EquivariantSet (EquivariantSet(..))
import Nominal
import Support


-- TODO: foldable / traversable
-- TODO: adjust / alter / update
-- TODO: -WithKey functions
-- TODO: don't export the helper functions
-- TODO: cleanup (usage of getElelemtE is not always necessary?)
-- TODO: replace [Bool] by Vec Bool if better?


-- Very similar to EquivariantSet, but then the map analogue. The important
-- thing is that we have to store which values are preserved under a map. This
-- is done with the list of bit vector. Otherwise, it is an orbit-wise
-- representation, just like sets.
newtype EquivariantMap k v = EqMap { unEqMap :: Map (Orbit k) (Orbit v, [Bool]) }

-- Need undecidableIntances for this
deriving instance (Eq (Orbit k), Eq (Orbit v)) => Eq (EquivariantMap k v)
deriving instance (Ord (Orbit k), Ord (Orbit v)) => Ord (EquivariantMap k v)
deriving instance (Show (Orbit k), Show (Orbit v)) => Show (EquivariantMap k v)

-- Defined by the join-semilattice structure of Map.
-- This is left biased.
deriving instance Ord (Orbit k) => Monoid (EquivariantMap k v)
deriving instance Ord (Orbit k) => Semigroup (EquivariantMap k v)

-- This action is trivial, since equivariant maps are equivariant
deriving via (Trivial (EquivariantMap k v)) instance Nominal (EquivariantMap k v) 


-- Query

null :: EquivariantMap k v -> Bool
null (EqMap m) = Map.null m

member :: (Nominal k, Ord (Orbit k)) => k -> EquivariantMap k v -> Bool
member x (EqMap m) = Map.member (toOrbit x) m

lookup :: (Nominal k, Ord (Orbit k), Nominal v) => k -> EquivariantMap k v -> Maybe v
lookup x (EqMap m) = mapelInv x <$> Map.lookup (toOrbit x) m

(!) :: (Nominal k, Ord (Orbit k), Nominal v) => EquivariantMap k v -> k -> v
(!) m k = fromMaybe undefined (EquivariantMap.lookup k m)

-- Construction

empty :: EquivariantMap k v
empty = EqMap Map.empty

singleton :: (Nominal k, Nominal v) => k -> v -> EquivariantMap k v
singleton k v = EqMap (Map.singleton (toOrbit k) (mapel k v))

insert :: (Nominal k, Nominal v, Ord (Orbit k)) => k -> v -> EquivariantMap k v -> EquivariantMap k v
insert k v (EqMap m) = EqMap (Map.insert (toOrbit k) (mapel k v) m)

delete :: (Nominal k, Ord (Orbit k)) => k -> EquivariantMap k v -> EquivariantMap k v
delete k (EqMap m) = EqMap (Map.delete (toOrbit k) m)


-- Combine

-- Can be done with just Map.unionWith and without getElementE but is a bit
-- harder (probably easier). Also true for related functions
-- op should be equivariant!
unionWith :: forall k v. (Nominal k, Nominal v, Ord (Orbit k)) => (v -> v -> v) -> EquivariantMap k v -> EquivariantMap k v -> EquivariantMap k v
unionWith op (EqMap m1) (EqMap m2) = EqMap (Map.unionWithKey opl m1 m2)
  where opl ko p1 p2 = let k = getElementE ko :: k in mapel k (mapelInv k p1 `op` mapelInv k p2)

intersectionWith :: forall k v1 v2 v3. (Nominal k, Nominal v1, Nominal v2, Nominal v3, Ord (Orbit k)) => (v1 -> v2 -> v3) -> EquivariantMap k v1 -> EquivariantMap k v2 -> EquivariantMap k v3
intersectionWith op (EqMap m1) (EqMap m2) = EqMap (Map.intersectionWithKey opl m1 m2)
  where opl ko p1 p2 = let k = getElementE ko :: k in mapel k (mapelInv k p1 `op` mapelInv k p2)


-- Traversal

-- f should be equivariant
map :: forall k v1 v2. (Nominal k, Nominal v1, Nominal v2) => (v1 -> v2) -> EquivariantMap k v1 -> EquivariantMap k v2
map f (EqMap m) = EqMap (Map.mapWithKey f2 m)
  where f2 ko p1 = let k = getElementE ko :: k in mapel k (f $ mapelInv k p1)

mapWithKey :: (Nominal k, Nominal v1, Nominal v2) => (k -> v1 -> v2) -> EquivariantMap k v1 -> EquivariantMap k v2
mapWithKey f (EqMap m) = EqMap (Map.mapWithKey f2 m)
  where f2 ko p1 = let k = getElementE ko in mapel k (f k $ mapelInv k p1)


-- Conversion

keysSet :: EquivariantMap k v -> EquivariantSet k
keysSet (EqMap m) = EqSet (Map.keysSet m)

-- f equivariant
fromSet :: (Nominal k, Nominal v) => (k -> v) -> EquivariantSet k -> EquivariantMap k v
fromSet f (EqSet s) = EqMap (Map.fromSet f2 s)
  where f2 ko = let k = getElementE ko in mapel k (f k)

toList :: (Nominal k, Nominal v) => EquivariantMap k v -> [(k, v)]
toList (EqMap l) = [(k, mapelInv k vob) | (ko, vob) <- Map.toList l, let k = getElementE ko]

fromList :: (Nominal k, Nominal v, Ord (Orbit k)) => [(k, v)] -> EquivariantMap k v
fromList l = EqMap . Map.fromList $ [(toOrbit k, mapel k v) | (k, v) <- l]

fromListWith :: forall k v. (Nominal k, Nominal v, Ord (Orbit k)) => (v -> v -> v) -> [(k, v)] -> EquivariantMap k v
fromListWith f l = EqMap . Map.fromListWithKey opf $ [(toOrbit k, mapel k v) | (k, v) <- l]
  where opf ko p1 p2 = let k = getElementE ko :: k in mapel k (mapelInv k p1 `f` mapelInv k p2)


-- Filter

filter :: forall k v. (Nominal k, Nominal v) => (v -> Bool) -> EquivariantMap k v -> EquivariantMap k v
filter p (EqMap m) = EqMap (Map.filterWithKey p2 m)
  where p2 ko pr = let k = getElementE ko :: k in p $ mapelInv k pr


-- Helper functions

mapel :: (Nominal k, Nominal v) => k -> v -> (Orbit v, [Bool])
mapel k v = (toOrbit v, bv (Support.toList (support k)) (Support.toList (support v)))

bv :: [Rat] -> [Rat] -> [Bool]
bv l [] = replicate (length l) False
bv [] _ = error "Non-equivariant function"
bv (x:xs) (y:ys) = case compare x y of
  LT -> False : bv xs (y:ys)
  EQ -> True : bv xs ys
  GT -> error "Non-equivariant function"

mapelInv :: (Nominal k, Nominal v) => k -> (Orbit v, [Bool]) -> v
mapelInv x (oy, bs) = getElement oy (Support.fromDistinctAscList . fmap fst . Prelude.filter snd $ zip (Support.toList (support x)) bs)

