{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EquivariantSet where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (Semigroup)

import Orbit
import Support

-- TODO: think about folds (the monoids should be nominal?)
-- TODO: partition / fromList / ...

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

-- We could derive a correct instance if I had written generic instances.
-- Didn't do that yet, but a direct instance is also nice.
instance Orbit (EquivariantSet a) where
  newtype Orb (EquivariantSet a) = OrbEqSet (EquivariantSet a)
  toOrbit = OrbEqSet
  support _ = Support.empty
  getElement (OrbEqSet x) _ = x
  index _ = 0

deriving instance Show (Orb a) => Show (Orb (EquivariantSet a))
deriving instance Eq (Orb a) => Eq (Orb (EquivariantSet a))
deriving instance Ord (Orb a) => Ord (Orb (EquivariantSet a))


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

-- This is the meat of the file! Relies on the ordering of Orbit.product
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

-- f should also preserve order on the orbit types!
-- This means you should know the representation to use it well
mapMonotonic :: (Orbit a, Orbit b) => (a -> b) -> EquivariantSet a -> EquivariantSet b
mapMonotonic f = EqSet . Set.mapMonotonic (toOrbit . f . getElementE) . unEqSet


-- Conversion

toList :: Orbit a => EquivariantSet a -> [a]
toList = fmap getElementE . Set.toList . unEqSet

