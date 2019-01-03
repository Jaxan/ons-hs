{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module EquivariantSet where

import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (map, product)

import Orbit


-- Given a nominal type, we can construct equivariant sets. These simply use a
-- standard set data structure. This works well because orbits are uniquely
-- represented. Although internally it is just a set of orbits, the interface
-- will always work directly with elements. This way we model infinite sets.
-- Note that functions such as toList do not return an ordered list since the
-- representatives are chosen arbitrarily.
newtype EquivariantSet a = EqSet { unEqSet :: Set (Orb a) }

-- Need undecidableIntances for this
deriving instance Eq (Orb a) => Eq (EquivariantSet a)
deriving instance Ord (Orb a) => Ord (EquivariantSet a)
deriving instance Show (Orb a) => Show (EquivariantSet a)

-- For these we rely on the instances of Set
-- It defines the join semi-lattice structure
deriving instance Ord (Orb a) => Monoid (EquivariantSet a)
deriving instance Ord (Orb a) => Semigroup (EquivariantSet a)

-- This action is trivial, since equivariant sets are equivariant
deriving via (Trivial (EquivariantSet a)) instance Orbit (EquivariantSet a) 


-- Query

null :: EquivariantSet a -> Bool
null = Set.null . unEqSet

orbits :: EquivariantSet a -> Int
orbits = Set.size . unEqSet

member :: (Orbit a, Ord (Orb a)) => a -> EquivariantSet a -> Bool
member a = Set.member (toOrbit a) . unEqSet

isSubsetOf :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> Bool
isSubsetOf (EqSet s1) (EqSet s2) = Set.isSubsetOf s1 s2


-- Construction

empty :: EquivariantSet a
empty = EqSet Set.empty

singleOrbit :: Orbit a => a -> EquivariantSet a
singleOrbit = EqSet . Set.singleton . toOrbit

-- Insert whole orbit of a
insert :: (Orbit a, Ord (Orb a)) => a -> EquivariantSet a -> EquivariantSet a
insert a = EqSet . Set.insert (toOrbit a) . unEqSet

-- Deletes whole orbit of a
delete :: (Orbit a, Ord (Orb a)) => a -> EquivariantSet a -> EquivariantSet a
delete a = EqSet . Set.delete (toOrbit a) . unEqSet


-- Combine

union :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
union a b = EqSet $ Set.union (unEqSet a) (unEqSet b)

-- Not symmetric, but A \ B
difference :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
difference a b = EqSet $ Set.difference (unEqSet a) (unEqSet b)

intersection :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
intersection a b = EqSet $ Set.intersection (unEqSet a) (unEqSet b)

-- Cartesian product. This is a non trivial thing and relies on the
-- ordering of Orbit.product.
product :: forall a b. (Orbit a, Orbit b) => EquivariantSet a -> EquivariantSet b -> EquivariantSet (a, b)
product (EqSet sa) (EqSet sb) = EqSet . Set.fromDistinctAscList . concat
                              $ Orbit.product (Proxy @a) (Proxy @b) <$> Set.toAscList sa <*> Set.toAscList sb

-- Cartesian product followed by a function (f should be equivariant)
productWith :: (Orbit a, Orbit b, Orbit c, Ord (Orb c)) => (a -> b -> c) -> EquivariantSet a -> EquivariantSet b -> EquivariantSet c
productWith f as bs = map (uncurry f) $ EquivariantSet.product as bs


-- Filter

-- f should be equivariant
filter :: Orbit a => (a -> Bool) -> EquivariantSet a -> EquivariantSet a
filter f (EqSet s) = EqSet . Set.filter (f . getElementE) $ s

-- f should be equivariant
partition :: Orbit a => (a -> Bool) -> EquivariantSet a -> (EquivariantSet a, EquivariantSet a)
partition f (EqSet s) = both EqSet . Set.partition (f . getElementE) $ s
  where both f (a, b) = (f a, f b)


-- Map

-- precondition: f is equivariant
-- Note that f may change the ordering
map :: (Orbit a, Orbit b, Ord (Orb b)) => (a -> b) -> EquivariantSet a -> EquivariantSet b
map f = EqSet . Set.map (omap f) . unEqSet

-- precondition: f quivariant and preserves order on the orbits.
-- This means you should know the representation to use it well
mapMonotonic :: (Orbit a, Orbit b) => (a -> b) -> EquivariantSet a -> EquivariantSet b
mapMonotonic f = EqSet . Set.mapMonotonic (omap f) . unEqSet


-- Folds

-- I am not sure about the preconditions for folds
foldr :: Orbit a => (a -> b -> b) -> b -> EquivariantSet a -> b
foldr f b = Set.foldr (f . getElementE) b . unEqSet

foldl :: Orbit a => (b -> a -> b) -> b -> EquivariantSet a -> b
foldl f b = Set.foldl (\b -> f b . getElementE) b . unEqSet


-- Conversion

toList :: Orbit a => EquivariantSet a -> [a]
toList = fmap getElementE . Set.toList . unEqSet

fromList :: (Orbit a, Ord (Orb a)) => [a] -> EquivariantSet a
fromList = EqSet . Set.fromList . fmap toOrbit

toOrbitList :: EquivariantSet a -> [Orb a]
toOrbitList = Set.toList . unEqSet

fromOrbitList :: Ord (Orb a) => [Orb a] -> EquivariantSet a
fromOrbitList = EqSet . Set.fromList
