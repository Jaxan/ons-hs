{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nominal.Class
  ( Nominal(..) -- typeclass
  , Trivially(..) -- newtype wrapper for deriving instances
  , Generically(..) -- newtype wrapper for deriving instances
  , OrbPair(..) -- need to export?
  , OrbRec(..)  -- need to export?
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Void
import GHC.Generics

import Support


-- This is the main meat of the package. The Nominal typeclass, it gives us ways
-- to manipulate nominal elements in sets and maps. The type class has
-- associated data to represent an orbit of type a. This is often much easier
-- than the type a itself. For example, all orbits of Rat are equal.
-- Furthermore, we provide means to go back and forth between elements and
-- orbits, and we get to know their support size. For many manipulations we
-- need an Ord instance on the associated data type, this can often be
-- implemented, even when the type 'a' does not have an Ord instance.
--
-- Laws / conditions:
-- * index . toOrbit == size . support
-- * getElement o s is defined if index o == Set.size s
class Nominal a where
  type Orbit a :: Type
  toOrbit    :: a -> Orbit a
  support    :: a -> Support
  getElement :: Orbit a -> Support -> a
  index      :: Proxy a -> Orbit a -> Int


-- We can construct orbits from rational numbers. There is exactly one orbit,
-- so this can be represented by the unit type.
instance Nominal Rat where
  type Orbit Rat = ()
  toOrbit _ = ()
  support r = Support.singleton r
  getElement _ s = Support.min s
  index _ _ = 1


-- Supports themselves are nominal. Note that this is a very important instance
-- as all other instances can reduce to this one (and perhaps the one for
-- products). 'Abstract types' in the original ONS library can be represented
-- directly as T = (Trivial Int, Support). The orbit of a given support is
-- completely specified by an integer.
instance Nominal Support where
  type Orbit Support = Int
  toOrbit s = Support.size s
  support s = s
  getElement _ s = s
  index _ n = n


-- Two general ways for deriving instances are provided:
-- 1. A trivial instance, where the group action is trivial. This means that
--    each value is its own orbit and is supported by the empty set.
-- 2. A generic instance, this uses the GHC.Generis machinery. This will
--    derive ``the right'' instance based on the algebraic data type.
-- Neither of them is a default, so they should be derived using DerivingVia.
-- (Available from GHC 8.6.1.)

-- For the trivial action, each element is its own orbit and is supported
-- by the empty set.
newtype Trivially a = Trivial a
instance Nominal (Trivially a) where
  type Orbit (Trivially a) = a
  toOrbit (Trivial a) = a
  support _ = Support.empty
  getElement a _ = Trivial a
  index _ _ = 0


-- We can now define trivial instances for some basic types. (Some of these
-- could equivalently be derived with generics.)
deriving via Trivially Void instance Nominal Void
deriving via Trivially () instance Nominal ()
deriving via Trivially Bool instance Nominal Bool
deriving via Trivially Char instance Nominal Char
deriving via Trivially Int instance Nominal Int -- NB: Trivial instance!
deriving via Trivially Ordering instance Nominal Ordering


-- The generic instance unfolds the algebraic data type in sums and products,
-- these have their own instances defined below.
instance (Generic a, GNominal (Rep a)) => Nominal (Generically a) where
  type Orbit (Generically a) = GOrbit (Rep a)
  toOrbit = gtoOrbit . from . unGenerically
  support = gsupport . from . unGenerically
  getElement o s = Generically (to (ggetElement o s))
  index _ = gindex (Proxy :: Proxy (Rep a))

-- Not exported
unGenerically :: Generically a -> a
unGenerically (Generically a) = a

-- Some instances we can derive via generics
deriving via Generically (a, b) instance (Nominal a, Nominal b) => Nominal (a, b)
deriving via Generically (a, b, c) instance (Nominal a, Nominal b, Nominal c) => Nominal (a, b, c)
deriving via Generically (a, b, c, d) instance (Nominal a, Nominal b, Nominal c, Nominal d) => Nominal (a, b, c, d)
deriving via Generically (Either a b) instance (Nominal a, Nominal b) => Nominal (Either a b)
deriving via Generically [a] instance Nominal a => Nominal [a]
deriving via Generically (Maybe a) instance Nominal a => Nominal (Maybe a)


-- Generic class, so that custom data types can be derived
class GNominal f where
  type GOrbit f :: Type
  gtoOrbit    :: f a -> GOrbit f
  gsupport    :: f a -> Support
  ggetElement :: GOrbit f -> Support -> f a
  gindex      :: Proxy f -> GOrbit f -> Int


-- Instance for the Void type
instance GNominal V1 where
  type GOrbit V1 = Void
  gtoOrbit _ = undefined
  gsupport _ = empty
  ggetElement _ _ = undefined
  gindex _ _ = 0


-- Instance for the Uni type
instance GNominal U1 where
  type GOrbit U1 = ()
  gtoOrbit _ = ()
  gsupport _ = empty
  ggetElement _ _ = U1
  gindex _ _ = 0


-- Disjoint unions are easy: just work on either side.
instance (GNominal f, GNominal g) => GNominal (f :+: g) where
  type GOrbit (f :+: g) = Either (GOrbit f) (GOrbit g)
  gtoOrbit (L1 a) = Left  (gtoOrbit a)
  gtoOrbit (R1 b) = Right (gtoOrbit b)
  gsupport (L1 a) = gsupport a
  gsupport (R1 b) = gsupport b
  ggetElement (Left  oa) s = L1 (ggetElement oa s)
  ggetElement (Right ob) s = R1 (ggetElement ob s)
  gindex proxy (Left  oa) = gindex (left proxy) oa where
    left :: proxy (f :+: g) -> Proxy f
    left _ = Proxy
  gindex proxy (Right ob) = gindex (right proxy) ob where
    right :: proxy (f :+: g) -> Proxy g
    right _ = Proxy


-- The cartesian product is a non-trivial instance. We represent orbits in a
-- product as described inthe paper: with two orbits, and how the match. The
-- matchings can be given as strings, which can be easily enumerated, in order
-- to enumerate the whole product.
instance (GNominal f, GNominal g) => GNominal (f :*: g) where
  type GOrbit (f :*: g) = OrbPair (GOrbit f) (GOrbit g)
  gtoOrbit ~(a :*: b) = OrbPair (gtoOrbit a) (gtoOrbit b) (bla sa sb)
    where
      sa = toList $ gsupport a
      sb = toList $ gsupport b
      bla [] ys = fmap (const GT) ys
      bla xs [] = fmap (const LT) xs
      bla (x:xs) (y:ys) = case compare x y of
        LT -> LT : (bla xs (y:ys))
        EQ -> EQ : (bla xs ys)
        GT -> GT : (bla (x:xs) ys)
  gsupport ~(a :*: b) = (gsupport a) `union` (gsupport b)
  ggetElement (OrbPair oa ob l) s = (ggetElement oa $ toSet ls) :*: (ggetElement ob $ toSet rs)
    where
      ~(ls, rs) = partitionOrd fst . zip l . toList $ s
      toSet = fromDistinctAscList . fmap snd
  gindex _ (OrbPair _ _ l) = length l

data OrbPair a b = OrbPair !a !b ![Ordering]
  deriving (Eq, Ord, Show, Generic)

-- Could be in prelude or some other general purpose lib
partitionOrd :: (a -> Ordering) -> [a] -> ([a], [a])
partitionOrd p xs = foldr (selectOrd p) ([], []) xs

selectOrd :: (a -> Ordering) -> a -> ([a], [a]) -> ([a], [a])
selectOrd f x ~(ls, rs) = case f x of
  LT -> (x : ls, rs)
  EQ -> (x : ls, x : rs)
  GT -> (ls, x : rs)


instance Nominal a => GNominal (K1 c a) where
  -- Cannot use (Orb a) here, that may lead to a recursive type
  -- So we use the type OrbRec a instead (which uses Orb a one step later).
  type GOrbit (K1 c a) = OrbRec a
  gtoOrbit (K1 x) = OrbRec (toOrbit x)
  gsupport (K1 x) = support x
  ggetElement (OrbRec x) s = K1 $ getElement x s
  gindex _ (OrbRec o) = index (Proxy :: Proxy a) o

newtype OrbRec a = OrbRec (Orbit a)
  deriving Generic
deriving instance Eq (Orbit a) => Eq (OrbRec a)
deriving instance Ord (Orbit a) => Ord (OrbRec a)
deriving instance Show (Orbit a) => Show (OrbRec a)


instance GNominal f => GNominal (M1 i c f) where
  type GOrbit (M1 i c f) = GOrbit f
  gtoOrbit (M1 x) = gtoOrbit x
  gsupport (M1 x) = gsupport x
  ggetElement x s = M1 $ ggetElement x s
  gindex _ o = gindex (Proxy :: Proxy f) o
