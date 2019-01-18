{-# language FlexibleContexts #-}
module OnsQuotient where

import Nominal (Nominal(..))
import Support (Support, intersect)
import OrbitList
import EquivariantMap (EquivariantMap(..))
import qualified EquivariantMap as Map
import EquivariantSet (EquivariantSet(..))
import qualified EquivariantSet as Set

import Prelude (Bool, Int, Ord, (.), (<>), (+), ($), snd, fmap, uncurry)

type QuotientType = (Int, Support)
type QuotientMap a = EquivariantMap a QuotientType

-- Computes a quotient map given an equivalence relation
quotient :: (Nominal a, Ord (Orbit a)) => EquivariantSet (a, a) -> OrbitList a -> (QuotientMap a, OrbitList QuotientType)
quotient equiv = quotientf (\a b -> (a, b) `Set.member` equiv)

-- f should be equivariant and an equivalence relation
quotientf :: (Nominal a, Ord (Orbit a)) => (a -> a -> Bool) -> OrbitList a -> (QuotientMap a, OrbitList QuotientType)
quotientf f ls = go 0 Map.empty empty (toList ls)
  where
    go _ phi acc []     = (phi, acc)
    go n phi acc (a:as) = 
      let y0 = filter (uncurry f) (product (singleOrbit a) (fromList as))
          y1 = filter (uncurry f) (product (singleOrbit a) (singleOrbit a))
          y2 = map (\(a1, a2) -> (a2, (n, support a1 `intersect` support a2))) (y1 <> y0)
          m0 = Map.fromListWith (\(n1, s1) (n2, s2) -> (n1, s1 `intersect` s2)) . toList $ y2
          l0 = take 1 . fromList . fmap snd $ Map.toList m0
      in if a `Set.member` (Map.keysSet phi)
         then go n phi acc as
         else go (n+1) (phi <> m0) (acc <> l0) as
