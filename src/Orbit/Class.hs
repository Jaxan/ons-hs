{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Orbit.Class where

import Data.Void
import Data.Proxy (Proxy(..))
import GHC.Generics

import Support

-- This is the main meat of the package. The Orbit typeclass, it gives us ways
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
class Orbit a where
  type Orb a :: *
  toOrbit :: a -> Orb a
  support :: a -> Support
  getElement :: Orb a -> Support -> a
  index :: Proxy a -> Orb a -> Int

  -- default Orb a :: (Generic a, GOrbit (Rep a)) => *
  type Orb a = GOrb (Rep a)

  default toOrbit :: (Generic a, GOrbit (Rep a), Orb a ~ GOrb (Rep a)) => a -> Orb a
  toOrbit = gtoOrbit . from

  default support :: (Generic a, GOrbit (Rep a), Orb a ~ GOrb (Rep a)) => a -> Support
  support = gsupport . from

  default getElement :: (Generic a, GOrbit (Rep a), Orb a ~ GOrb (Rep a)) => Orb a -> Support -> a
  getElement o s = to (ggetElement o s)

  default index :: (Generic a, GOrbit (Rep a), Orb a ~ GOrb (Rep a)) => Proxy a -> Orb a -> Int
  index _ = gindex (Proxy :: Proxy (Rep a))

  {-# INLINABLE toOrbit #-}
  {-# INLINABLE support #-}
  {-# INLINABLE getElement #-}
  {-# INLINABLE index #-}


-- Generic class, so that custom data types can be derived
class GOrbit f where
  type GOrb f :: *
  gtoOrbit :: f a -> GOrb f
  gsupport :: f a -> Support
  ggetElement :: GOrb f -> Support -> f a
  gindex :: Proxy f -> GOrb f -> Int


-- Instance for the Void type
instance GOrbit V1 where
  type GOrb V1 = Void
  gtoOrbit v = undefined
  gsupport _ = empty
  ggetElement v _ = undefined
  gindex _ _ = 0


-- Instance for the Uni type
instance GOrbit U1 where
  type GOrb U1 = ()
  gtoOrbit _ = ()
  gsupport _ = empty
  ggetElement _ _ = U1
  gindex _ _ = 0


-- Disjoint unions are easy: just work on either side.
instance (GOrbit f, GOrbit g) => GOrbit (f :+: g) where
  type GOrb (f :+: g) = Either (GOrb f) (GOrb g)
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
instance (GOrbit f, GOrbit g) => GOrbit (f :*: g) where
  type GOrb (f :*: g) = OrbPair (GOrb f) (GOrb g)
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
  deriving (Show, Eq, Ord, Generic)

-- Could be in prelude or some other general purpose lib
partitionOrd :: (a -> Ordering) -> [a] -> ([a], [a])
partitionOrd p xs = foldr (selectOrd p) ([], []) xs

selectOrd :: (a -> Ordering) -> a -> ([a], [a]) -> ([a], [a])
selectOrd f x ~(ls, rs) = case f x of
  LT -> (x : ls, rs)
  EQ -> (x : ls, x : rs)
  GT -> (ls, x : rs)


instance Orbit a => GOrbit (K1 c a) where
  type GOrb (K1 c a) = OrbRec a
  gtoOrbit (K1 x) = OrbRec (toOrbit x)
  gsupport (K1 x) = support x
  ggetElement (OrbRec x) s = K1 $ getElement x s
  gindex p (OrbRec o) = index (Proxy :: Proxy a) o

newtype OrbRec a = OrbRec (Orb a)
  deriving (Generic)
deriving instance Show (Orb a) => Show (OrbRec a)
deriving instance Ord (Orb a) => Ord (OrbRec a)
deriving instance Eq (Orb a) => Eq (OrbRec a)


instance GOrbit f => GOrbit (M1 i c f) where
  type GOrb (M1 i c f) = GOrb f
  gtoOrbit (M1 x) = gtoOrbit x
  gsupport (M1 x) = gsupport x
  ggetElement x s = M1 $ ggetElement x s
  gindex p o = gindex (Proxy :: Proxy f) o
