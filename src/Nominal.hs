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

-- General combinator
productG :: (Nominal a, Nominal b) => (Int -> Int -> [[Ordering]]) -> Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
productG strs pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> strs (index pa oa) (index pb ob)

-- Enumerate all orbits in a product A x B.
product :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
product = productG prodStrings

-- Separated product: A * B = { (a,b) | Exist C1, C2 disjoint supporting a, b resp.}
separatedProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
separatedProduct = productG sepProdStrings

-- "Left product": A ⫂ B = { (a,b) | C supports a => C supports b }
leftProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
leftProduct = productG lsupprProdStrings

-- "Right product": A ⫁ B = { (a,b) | C supports a <= C supports b }
rightProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
rightProduct = productG rsupplProdStrings

-- Strictly increasing product = { (a,b) | all elements in a < all elements in b }
increasingProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
increasingProduct = productG incrSepProdStrings

-- Strictly decreasing product = { (a,b) | all elements in a > elements in b }
decreasingProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a,b)]
decreasingProduct = productG decrSepProdStrings
