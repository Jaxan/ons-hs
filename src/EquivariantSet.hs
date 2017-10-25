{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EquivariantSet where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (Semigroup)

import Orbit

-- Given a nominal type, we can construct equivariant sets
-- These simply use a set data structure from prelude
-- This works well because orbits are uniquely represented
-- Note that functions such as toList do not return an ordered
-- list since the representatives are chosen arbitrarily.
-- TODO: think about folds (and size)
newtype EquivariantSet a = EqSet { unEqSet :: Set (Orb a) }

-- Need undecidableIntances for this
deriving instance Eq (Orb a) => Eq (EquivariantSet a)
deriving instance Ord (Orb a) => Ord (EquivariantSet a)
deriving instance Show (Orb a) => Show (EquivariantSet a)

-- For these we rely on the instances of Set
-- It defines the join semi-lattice structure
deriving instance Ord (Orb a) => Monoid (EquivariantSet a)
deriving instance Ord (Orb a) => Semigroup (EquivariantSet a)


-- Query

null :: EquivariantSet a -> Bool
null = Set.null . unEqSet

size :: EquivariantSet a -> Int
size = Set.size . unEqSet

member :: (Orbit a, Ord (Orb a)) => a -> EquivariantSet a -> Bool
member a = Set.member (toOrbit a) . unEqSet

isSubsetOf :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> Bool
isSubsetOf (EqSet s1) (EqSet s2) = Set.isSubsetOf s1 s2


-- Construction

empty :: EquivariantSet a
empty = EqSet Set.empty

singleOrbit :: Orbit a => a -> EquivariantSet a
singleOrbit = EqSet . Set.singleton . toOrbit

insert :: (Orbit a, Ord (Orb a)) => a -> EquivariantSet a -> EquivariantSet a
insert a = EqSet . Set.insert (toOrbit a) . unEqSet

delete :: (Orbit a, Ord (Orb a)) => a -> EquivariantSet a -> EquivariantSet a
delete a = EqSet . Set.delete (toOrbit a) . unEqSet


-- Combine

union :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
union a b = EqSet $ Set.union (unEqSet a) (unEqSet b)

difference :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
difference a b = EqSet $ Set.difference (unEqSet a) (unEqSet b)

intersection :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
intersection a b = EqSet $ Set.intersection (unEqSet a) (unEqSet b)

-- This is the meat of the file!
product :: (Orbit a, Orbit b) => EquivariantSet a -> EquivariantSet b -> EquivariantSet (a, b)
product (EqSet sa) (EqSet sb) = EqSet . Set.fromDistinctAscList . concat
                              $ Orbit.product <$> Set.toAscList sa <*> Set.toAscList sb


-- Filter

-- f should be equivariant
filter :: Orbit a => (a -> Bool) -> EquivariantSet a -> EquivariantSet a
filter f (EqSet s) = EqSet . Set.filter (f . getElementE) $ s


-- Map

-- precondition: f is equivariant
-- Note that f may change the ordering
map :: (Orbit a, Orbit b, Ord (Orb b)) => (a -> b) -> EquivariantSet a -> EquivariantSet b
map f = EqSet . Set.map (toOrbit . f . getElementE) . unEqSet

-- f should also preserve order!
mapMonotonic :: (Orbit a, Orbit b) => (a -> b) -> EquivariantSet a -> EquivariantSet b
mapMonotonic f = EqSet . Set.mapMonotonic (toOrbit . f . getElementE) . unEqSet


-- Conversion

toList :: Orbit a => EquivariantSet a -> [a]
toList = fmap getElementE . Set.toList . unEqSet

