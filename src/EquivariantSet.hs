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

import Nominal
import OrbitList (OrbitList(..))


-- Given a nominal type, we can construct equivariant sets. These simply use a
-- standard set data structure. This works well because orbits are uniquely
-- represented. Although internally it is just a set of orbits, the interface
-- will always work directly with elements. This way we model infinite sets.
-- Note that functions such as toList do not return an ordered list since the
-- representatives are chosen arbitrarily. This action is trivial, since
-- equivariant sets are equivariant :-).
newtype EquivariantSet a = EqSet { unEqSet :: Set (Orbit a) }
  deriving Nominal via Trivial (EquivariantSet a)

-- Need undecidableIntances for this
deriving instance Eq (Orbit a) => Eq (EquivariantSet a)
deriving instance Ord (Orbit a) => Ord (EquivariantSet a)
deriving instance Show (Orbit a) => Show (EquivariantSet a)

-- For these we rely on the instances of Set
-- It defines the join semi-lattice structure
deriving instance Ord (Orbit a) => Monoid (EquivariantSet a)
deriving instance Ord (Orbit a) => Semigroup (EquivariantSet a)


-- Query

null :: EquivariantSet a -> Bool
null = Set.null . unEqSet

orbits :: EquivariantSet a -> Int
orbits = Set.size . unEqSet

member :: (Nominal a, Ord (Orbit a)) => a -> EquivariantSet a -> Bool
member a = Set.member (toOrbit a) . unEqSet

isSubsetOf :: Ord (Orbit a) => EquivariantSet a -> EquivariantSet a -> Bool
isSubsetOf (EqSet s1) (EqSet s2) = Set.isSubsetOf s1 s2


-- Construction

empty :: EquivariantSet a
empty = EqSet Set.empty

singleOrbit :: Nominal a => a -> EquivariantSet a
singleOrbit = EqSet . Set.singleton . toOrbit

-- Insert whole orbit of a
insert :: (Nominal a, Ord (Orbit a)) => a -> EquivariantSet a -> EquivariantSet a
insert a = EqSet . Set.insert (toOrbit a) . unEqSet

-- Deletes whole orbit of a
delete :: (Nominal a, Ord (Orbit a)) => a -> EquivariantSet a -> EquivariantSet a
delete a = EqSet . Set.delete (toOrbit a) . unEqSet


-- Combine

union :: Ord (Orbit a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
union a b = EqSet $ Set.union (unEqSet a) (unEqSet b)

-- Not symmetric, but A \ B
difference :: Ord (Orbit a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
difference a b = EqSet $ Set.difference (unEqSet a) (unEqSet b)

intersection :: Ord (Orbit a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
intersection a b = EqSet $ Set.intersection (unEqSet a) (unEqSet b)

-- Cartesian product. This is a non trivial thing and relies on the
-- ordering of Orbit.product.
product :: forall a b. (Nominal a, Nominal b) => EquivariantSet a -> EquivariantSet b -> EquivariantSet (a, b)
product (EqSet sa) (EqSet sb) = EqSet . Set.fromDistinctAscList . concat
                              $ Nominal.product (Proxy @a) (Proxy @b) <$> Set.toAscList sa <*> Set.toAscList sb

-- Cartesian product followed by a function (f should be equivariant)
productWith :: (Nominal a, Nominal b, Nominal c, Ord (Orbit c)) => (a -> b -> c) -> EquivariantSet a -> EquivariantSet b -> EquivariantSet c
productWith f as bs = map (uncurry f) $ EquivariantSet.product as bs


-- Filter

-- f should be equivariant
filter :: Nominal a => (a -> Bool) -> EquivariantSet a -> EquivariantSet a
filter f (EqSet s) = EqSet . Set.filter (f . getElementE) $ s

-- f should be equivariant
partition :: Nominal a => (a -> Bool) -> EquivariantSet a -> (EquivariantSet a, EquivariantSet a)
partition f (EqSet s) = both EqSet . Set.partition (f . getElementE) $ s
  where both g (a, b) = (g a, g b)


-- Map

-- precondition: f is equivariant
-- Note that f may change the ordering
map :: (Nominal a, Nominal b, Ord (Orbit b)) => (a -> b) -> EquivariantSet a -> EquivariantSet b
map f = EqSet . Set.map (omap f) . unEqSet

-- precondition: f quivariant and preserves order on the orbits.
-- This means you should know the representation to use it well
mapMonotonic :: (Nominal a, Nominal b) => (a -> b) -> EquivariantSet a -> EquivariantSet b
mapMonotonic f = EqSet . Set.mapMonotonic (omap f) . unEqSet


-- Folds

-- I am not sure about the preconditions for folds
foldr :: Nominal a => (a -> b -> b) -> b -> EquivariantSet a -> b
foldr f b = Set.foldr (f . getElementE) b . unEqSet

foldl :: Nominal a => (b -> a -> b) -> b -> EquivariantSet a -> b
foldl f b = Set.foldl (\acc -> f acc . getElementE) b . unEqSet


-- Conversion

toList :: Nominal a => EquivariantSet a -> [a]
toList = fmap getElementE . Set.toList . unEqSet

fromList :: (Nominal a, Ord (Orbit a)) => [a] -> EquivariantSet a
fromList = EqSet . Set.fromList . fmap toOrbit

toOrbitList :: EquivariantSet a -> OrbitList a
toOrbitList = OrbitList . Set.toList . unEqSet

fromOrbitList :: Ord (Orbit a) => OrbitList a -> EquivariantSet a
fromOrbitList = EqSet . Set.fromList . unOrbitList
