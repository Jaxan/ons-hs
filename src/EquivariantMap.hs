{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EquivariantMap where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (Semigroup)
import Data.Map (Map)
import qualified Data.Map as Map

import EquivariantSet (EquivariantSet(EqSet))
import Orbit

-- TODO: foldable / traversable
-- TODO: adjust / alter / update
-- TODO: -WithKey functions
-- TODO: don't export the helper functions
-- TODO: cleanup (usage of getElelemtE is not necessary)
-- TODO: replace [Bool] by Vec Bool if better?

-- Very similar to EquivariantSet, but then the map analogue. The important
-- thing is that we have to store which values are preserved under a map. This
-- is done with the list of bit vector. Otherwise, it is an orbit-wise
-- representation, just like sets.
newtype EquivariantMap k v = EqMap { unEqMap :: Map (Orb k) (Orb v, [Bool]) }

-- Need undecidableIntances for this
deriving instance (Eq (Orb k), Eq (Orb v)) => Eq (EquivariantMap k v)
deriving instance (Ord (Orb k), Ord (Orb v)) => Ord (EquivariantMap k v)
deriving instance (Show (Orb k), Show (Orb v)) => Show (EquivariantMap k v)

-- Left biased...
deriving instance Ord (Orb k) => Monoid (EquivariantMap k v)
deriving instance Ord (Orb k) => Semigroup (EquivariantMap k v)

-- Helper functions

mapel :: (Orbit k, Orbit v) => k -> v -> (Orb v, [Bool])
mapel k v = (toOrbit v, bv (Set.toAscList (support k)) (Set.toAscList (support v)))

bv :: [Rat] -> [Rat] -> [Bool]
bv l [] = replicate (length l) False
bv [] l = error "Non-equivariant function"
bv (x:xs) (y:ys) = case compare x y of
  LT -> False : bv xs (y:ys)
  EQ -> True : bv xs ys
  GT -> error "Non-equivariant function"

mapelInv :: (Orbit k, Orbit v) => k -> (Orb v, [Bool]) -> v
mapelInv x (oy, bv) = getElement oy (Set.fromAscList . fmap fst . Prelude.filter snd $ zip (Set.toAscList (support x)) bv)


-- Query

null :: EquivariantMap k v -> Bool
null (EqMap m) = Map.null m

member :: (Orbit k, Ord (Orb k)) => k -> EquivariantMap k v -> Bool
member x (EqMap m) = Map.member (toOrbit x) m

lookup :: (Orbit k, Ord (Orb k), Orbit v) => k -> EquivariantMap k v -> Maybe v
lookup x (EqMap m) = mapelInv x <$> Map.lookup (toOrbit x) m


-- Construction

empty :: EquivariantMap k v
empty = EqMap Map.empty

singleton :: (Orbit k, Orbit v) => k -> v -> EquivariantMap k v
singleton k v = EqMap (Map.singleton (toOrbit k) (mapel k v))

insert :: (Orbit k, Orbit v, Ord (Orb k)) => k -> v -> EquivariantMap k v -> EquivariantMap k v
insert k v (EqMap m) = EqMap (Map.insert (toOrbit k) (mapel k v) m)

delete :: (Orbit k, Ord (Orb k)) => k -> EquivariantMap k v -> EquivariantMap k v
delete k (EqMap m) = EqMap (Map.delete (toOrbit k) m)


-- Combine

-- Can be done with just Map.unionWith and without getElementE but is a bit
-- harder (probably easier). Also true for related functions
-- op should be equivariant!
unionWith :: (Orbit k, Orbit v, Ord (Orb k)) => (v -> v -> v) -> EquivariantMap k v -> EquivariantMap k v -> EquivariantMap k v
unionWith op (EqMap m1) (EqMap m2) = EqMap (Map.unionWithKey opl m1 m2)
  where opl ko p1 p2 = let k = getElementE ko in mapel k (mapelInv k p1 `op` mapelInv k p2)

intersectionWith :: (Orbit k, Orbit v1, Orbit v2, Orbit v3, Ord (Orb k)) => (v1 -> v2 -> v3) -> EquivariantMap k v1 -> EquivariantMap k v2 -> EquivariantMap k v3
intersectionWith op (EqMap m1) (EqMap m2) = EqMap (Map.intersectionWithKey opl m1 m2)
  where opl ko p1 p2 = let k = getElementE ko in mapel k (mapelInv k p1 `op` mapelInv k p2)


-- Traversal

-- f should be equivariant
map :: (Orbit k, Orbit v1, Orbit v2) => (v1 -> v2) -> EquivariantMap k v1 -> EquivariantMap k v2
map f (EqMap m) = EqMap (Map.mapWithKey f2 m)
  where f2 ko p1 = let k = getElementE ko in mapel k (f $ mapelInv k p1)

mapWithKey :: (Orbit k, Orbit v1, Orbit v2) => (k -> v1 -> v2) -> EquivariantMap k v1 -> EquivariantMap k v2
mapWithKey f (EqMap m) = EqMap (Map.mapWithKey f2 m)
  where f2 ko p1 = let k = getElementE ko in mapel k (f k $ mapelInv k p1)


-- Conversion

keysSet :: EquivariantMap k v -> EquivariantSet k
keysSet (EqMap m) = EqSet (Map.keysSet m)

fromSet :: (Orbit k, Orbit v) => (k -> v) -> EquivariantSet k -> EquivariantMap k v
fromSet f (EqSet s) = EqMap (Map.fromSet f2 s)
  where f2 ko = let k = getElementE ko in mapel k (f k)


-- Filter

filter :: (Orbit k, Orbit v) => (v -> Bool) -> EquivariantMap k v -> EquivariantMap k v
filter p (EqMap m) = EqMap (Map.filterWithKey p2 m)
  where p2 ko pr = let k = getElementE ko in p $ mapelInv k pr
