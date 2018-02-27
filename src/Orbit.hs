{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Orbit
  ( module Orbit
  , module Orbit.Class
  ) where

import Support (Support, Rat(..))
import qualified Support

import Orbit.Products
import Orbit.Class

-- TODO: Make generic instances (we already have sums and products)
-- TODO: For products: replace [Ordering] with Vec Ordering if better
-- TODO: replace Support by an ordered vector / list for speed?


-- We can get 'default' values, if we don't care about the support.
getElementE :: Orbit a => Orb a -> a
getElementE orb = getElement orb (Support.def (index orb))

-- We can `map` orbits to orbits for equivariant functions
omap :: (Orbit a, Orbit b) => (a -> b) -> Orb a -> Orb b
omap f = toOrbit . f . getElementE

-- We can construct orbits from rational numbers. There is exactly one orbit,
-- so this can be represented by the unit type.
instance Orbit Rat where
  data Orb Rat = OrbRational
  toOrbit _ = OrbRational
  support r = Support.singleton r
  getElement _ s = Support.min s
  index _ = 1

deriving instance Show (Orb Rat)
deriving instance Eq (Orb Rat)
deriving instance Ord (Orb Rat)


-- Supports themselves are nominal. Note that this is a very important instance
-- as all other instances can reduce to this one (and perhaps the one for
-- products). 'Abstract types' in the original ONS library can be represented
-- directly as T = (Trivial Int, Support). The orbit of a given support is
-- completely specified by an integer.
instance Orbit Support where
  newtype Orb Support = OrbSupport Int
  toOrbit s = OrbSupport (Support.size s)
  support s = s
  getElement _ s = s
  index (OrbSupport n) = n

deriving instance Show (Orb Support)
deriving instance Eq (Orb Support)
deriving instance Ord (Orb Support)


-- Disjoint unions are easy: just work on either side.
instance (Orbit a, Orbit b) => Orbit (Either a b) where
  newtype Orb (Either a b) = OrbEither (Either (Orb a) (Orb b))
  toOrbit (Left a) = OrbEither (Left (toOrbit a))
  toOrbit (Right b) = OrbEither (Right (toOrbit b))
  support (Left a) = support a
  support (Right b) = support b
  getElement (OrbEither (Left oa)) s = Left (getElement oa s)
  getElement (OrbEither (Right ob)) s = Right (getElement ob s)
  index (OrbEither (Left oa)) = index oa
  index (OrbEither (Right ob)) = index ob

deriving instance (Show (Orb a), Show (Orb b)) => Show (Orb (Either a b))
deriving instance (Eq (Orb a), Eq (Orb b)) => Eq (Orb (Either a b))
deriving instance (Ord (Orb a), Ord (Orb b)) => Ord (Orb (Either a b))


-- The cartesian product is a non-trivial instance. We represent orbits in a
-- product as described inthe paper: with two orbits, and how the match. The
-- matchings can be given as strings, which can be easily enumerated, in order
-- to enumerate the whole product.
instance (Orbit a, Orbit b) => Orbit (a, b) where
  data Orb (a,b) = OrbPair !(Orb a) !(Orb b) ![Ordering]
  toOrbit (a, b) = OrbPair (toOrbit a) (toOrbit b) (bla sa sb) 
    where
      sa = Support.toList $ support a
      sb = Support.toList $ support b
      bla [] ys = fmap (const GT) ys
      bla xs [] = fmap (const LT) xs
      bla (x:xs) (y:ys) = case compare x y of
        LT -> LT : (bla xs (y:ys))
        EQ -> EQ : (bla xs ys)
        GT -> GT : (bla (x:xs) ys)
  support (a, b) = Support.union (support a) (support b)
  getElement (OrbPair oa ob l) s = (getElement oa $ toSet ls, getElement ob $ toSet rs)
    where
      (ls, rs) = partitionOrd fst . zip l . Support.toList $ s
      toSet = Support.fromDistinctAscList . fmap snd
  index (OrbPair _ _ l) = length l

deriving instance (Show (Orb a), Show (Orb b)) => Show (Orb (a, b))
deriving instance (Eq (Orb a), Eq (Orb b)) => Eq (Orb (a, b))
deriving instance (Ord (Orb a), Ord (Orb b)) => Ord (Orb (a, b))

-- Could be in prelude or some other general purpose lib
{-# INLINABLE partitionOrd #-}
partitionOrd :: (a -> Ordering) -> [a] -> ([a], [a])
partitionOrd p xs = foldr (selectOrd p) ([], []) xs

selectOrd :: (a -> Ordering) -> a -> ([a], [a]) -> ([a], [a])
selectOrd f x ~(ls, rs) = case f x of
  LT -> (x : ls, rs)
  EQ -> (x : ls, x : rs)
  GT -> (ls, x : rs)

-- Enumerate all orbits in a product A x B. In lexicographical order!
product :: (Orbit a, Orbit b) => Orb a -> Orb b -> [Orb (a, b)]
product oa ob = OrbPair oa ob <$> prodStrings (index oa) (index ob)

-- Separated product: A * B = { (a,b) | Exist C1, C2 disjoint supporting a, b resp.}
separatedProduct :: (Orbit a, Orbit b) => Orb a -> Orb b -> [Orb (a, b)]
separatedProduct oa ob = OrbPair oa ob <$> sepProdStrings (index oa) (index ob)

-- "Left product": A |x B = { (a,b) | C supports a => C supports b }
leftProduct :: (Orbit a, Orbit b) => Orb a -> Orb b -> [Orb (a, b)]
leftProduct oa ob = OrbPair oa ob <$> rincProdStrings (index oa) (index ob)

{-# INLINABLE product #-}
{-# INLINABLE separatedProduct #-}
{-# INLINABLE leftProduct #-}


-- Data structure for the discrete nominal sets with a trivial action.
newtype Trivial a = Trivial { unTrivial :: a }
  deriving (Eq, Ord, Show)

-- We need to remember the value!
instance Orbit (Trivial a) where
  newtype Orb (Trivial a) = OrbTrivial a
  toOrbit (Trivial a) = OrbTrivial a
  support _ = Support.empty
  getElement (OrbTrivial a) _ = Trivial a
  index _ = 0

deriving instance Show a => Show (Orb (Trivial a))
deriving instance Eq a => Eq (Orb (Trivial a))
deriving instance Ord a => Ord (Orb (Trivial a))


-- Orbits themselves are trivial.
instance Orbit a => Orbit (Orb a) where
  newtype Orb (Orb a) = OrbOrb (Orb a)
  toOrbit a = OrbOrb a
  support _ = Support.empty
  getElement (OrbOrb oa) _ = oa
  index _ = 0

deriving instance Show (Orb a) => Show (Orb (Orb a))
deriving instance Eq (Orb a) => Eq (Orb (Orb a))
deriving instance Ord (Orb a) => Ord (Orb (Orb a))
