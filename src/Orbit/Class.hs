{-# LANGUAGE TypeFamilies #-}

module Orbit.Class where

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
-- * index . toOrbit == Set.size . support
-- * getElement o s is defined if index o == Set.size s
class Orbit a where
  data Orb a :: *
  toOrbit :: a -> Orb a
  support :: a -> Support
  getElement :: Orb a -> Support -> a
  index :: Orb a -> Int


{-
I tried to do generics, but failed. One cannot do generic injective
data constructors. I will keep it here now, for later reference.

{-# language DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
import GHC.Generic

  default Orb a :: (Generic a, GOrbit (Rep a)) => *
  data Orb a = Orb () -- how to make a default data instance declaration?

  default toOrbit :: (Generic a, GOrbit (Rep a)) => a -> Orb a
  toOrbit = _ . gtoOrbit . from

  default support :: (Generic a, GOrbit (Rep a)) => a -> Support
  support = gsupport . from

  default getElement :: (Generic a, GOrbit (Rep a)) => Orb a -> Support -> a
  getElement = undefined

  default index :: (Generic a, GOrbit (Rep a)) => Orb a -> Int
  index = undefined

class GOrbit f where
  data GOrb f :: * -> *
  gtoOrbit :: f a -> GOrb f a
  gsupport :: f a -> Support
  ggetElement :: GOrb f a -> Support -> f a
  gindex :: GOrb f a -> Int
-}
