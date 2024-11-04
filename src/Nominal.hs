{-# LANGUAGE ScopedTypeVariables #-}

module Nominal
  ( module Nominal
  , module Nominal.Class
  ) where

import Data.Proxy

import Nominal.Products
import Nominal.Class
import Support (Rat, def)

type Atom = Rat

-- We can get 'default' values, if we don't care about the support.
getElementE :: forall a. Nominal a => Orbit a -> a
getElementE orb = getElement orb (def (index (Proxy :: Proxy a) orb))

-- We can `map` orbits to orbits for equivariant functions
omap :: (Nominal a, Nominal b) => (a -> b) -> Orbit a -> Orbit b
omap f = toOrbit . f . getElementE

-- Enumerate all orbits in a product A x B. In lexicographical order!
product :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
product pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> prodStrings (index pa oa) (index pb ob)

-- Separated product: A * B = { (a,b) | Exist C1, C2 disjoint supporting a, b resp.}
separatedProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
separatedProduct pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> sepProdStrings (index pa oa) (index pb  ob)

-- "Left product": A |x B = { (a,b) | C supports a => C supports b }
leftProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
leftProduct pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> rincProdStrings (index pa oa) (index pb ob)
