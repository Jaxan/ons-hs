{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Orbit
  ( module Orbit
  , module Orbit.Class
  ) where

import Data.Proxy

import Support (Support, Rat(..))
import qualified Support

import Orbit.Products
import Orbit.Class


-- We can get 'default' values, if we don't care about the support.
getElementE :: forall a. Orbit a => Orb a -> a
getElementE orb = getElement orb (Support.def (index (Proxy :: Proxy a) orb))

-- We can `map` orbits to orbits for equivariant functions
omap :: (Orbit a, Orbit b) => (a -> b) -> Orb a -> Orb b
omap f = toOrbit . f . getElementE

-- We can construct orbits from rational numbers. There is exactly one orbit,
-- so this can be represented by the unit type.
instance Orbit Rat where
  type Orb Rat = ()
  toOrbit _ = ()
  support r = Support.singleton r
  getElement _ s = Support.min s
  index _ _ = 1


-- Supports themselves are nominal. Note that this is a very important instance
-- as all other instances can reduce to this one (and perhaps the one for
-- products). 'Abstract types' in the original ONS library can be represented
-- directly as T = (Trivial Int, Support). The orbit of a given support is
-- completely specified by an integer.
instance Orbit Support where
  type Orb Support = Int
  toOrbit s = Support.size s
  support s = s
  getElement _ s = s
  index _ n = n


-- Some instances we can derive via generics
deriving instance (Orbit a, Orbit b) => Orbit (Either a b)

deriving instance Orbit ()
deriving instance (Orbit a, Orbit b) => Orbit (a, b)
deriving instance (Orbit a, Orbit b, Orbit c) => Orbit (a, b, c)
deriving instance (Orbit a, Orbit b, Orbit c, Orbit d) => Orbit (a, b, c, d)

deriving instance Orbit a => Orbit [a]
deriving instance Orbit a => Orbit (Maybe a)


-- Enumerate all orbits in a product A x B. In lexicographical order!
product :: (Orbit a, Orbit b) => Proxy a -> Proxy b -> Orb a -> Orb b -> [Orb (a,b)]
product pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> prodStrings (index pa oa) (index pb ob)

-- Separated product: A * B = { (a,b) | Exist C1, C2 disjoint supporting a, b resp.}
separatedProduct :: (Orbit a, Orbit b) => Proxy a -> Proxy b -> Orb a -> Orb b -> [Orb (a,b)]
separatedProduct pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> sepProdStrings (index pa oa) (index pb  ob)

-- "Left product": A |x B = { (a,b) | C supports a => C supports b }
leftProduct :: (Orbit a, Orbit b) => Proxy a -> Proxy b -> Orb a -> Orb b -> [Orb (a,b)]
leftProduct pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> rincProdStrings (index pa oa) (index pb ob)

{-# INLINABLE product #-}
{-# INLINABLE separatedProduct #-}
{-# INLINABLE leftProduct #-}


-- Data structure for the discrete nominal sets with a trivial action.
newtype Trivial a = Trivial { unTrivial :: a }
  deriving (Eq, Ord, Show)

-- We need to remember the value!
instance Orbit (Trivial a) where
  type Orb (Trivial a) = a
  toOrbit (Trivial a) = a
  support _ = Support.empty
  getElement a _ = Trivial a
  index _ _ = 0
