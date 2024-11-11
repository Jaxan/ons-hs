{-# LANGUAGE ScopedTypeVariables #-}

module Nominal (
  -- * Atoms
  -- | Re-exports from "Nominal.Atom".
  Atom,
  atom,
  -- * Support
  -- | Re-exports from "Nominal.Support".
  Support,
  -- * The Nominal type class
  Nominal (..),
  Trivially (..),
  Generically (..),
  -- * Helper functions
  module Nominal,
) where

import Data.Proxy

import Nominal.Atom
import Nominal.Class
import Nominal.Products
import Nominal.Support

-- | We can construct a "default" element from an orbit. In this case, the
-- support is chosen arbitrarily.
getElementE :: forall a. Nominal a => Orbit a -> a
getElementE orb = getElement orb (def (index (Proxy :: Proxy a) orb))

-- | We can `map` orbits to orbits for equivariant functions.
omap :: (Nominal a, Nominal b) => (a -> b) -> Orbit a -> Orbit b
omap f = toOrbit . f . getElementE
