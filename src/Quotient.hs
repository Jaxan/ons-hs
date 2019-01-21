{-# language FlexibleContexts #-}
module Quotient where

import Nominal (Nominal(..))
import Support (Support, intersect)
import OrbitList
import EquivariantMap (EquivariantMap)
import qualified EquivariantMap as Map
import EquivariantSet (EquivariantSet)
import qualified EquivariantSet as Set

import Prelude (Bool, Int, Ord, (.), (<>), (+), ($), fst, snd, fmap, uncurry)


{- Computes the quotient of some set (given as OrbitList) and equivalence
   relations. Returns the quotientmap and the set of images. The non-trivial
   part is computing the least support, this is done by iteratively
   intersecting supports. -}

type QuotientType = (Int, Support)
type QuotientMap a = EquivariantMap a QuotientType

-- Computes a quotient map given an equivalence relation
quotient :: (Nominal a, Ord (Orbit a))
         => EquivariantSet (a, a) -> OrbitList a -> (QuotientMap a, OrbitList QuotientType)
quotient equiv = post . quotientf 0 (\a b -> (a, b) `Set.member` equiv)
  where post (a, b, _) = (a, map fst b)

-- f should be equivariant and an equivalence relation
quotientf :: (Nominal a, Ord (Orbit a))
          => Int -> (a -> a -> Bool) -> OrbitList a -> (QuotientMap a, OrbitList (QuotientType, EquivariantSet a), Int)
quotientf k f ls = go k Map.empty empty (toList ls)
  where
    go n phi acc []     = (phi, acc, n)
    go n phi acc (a:as) = 
      let y0 = filter (uncurry f) (product (singleOrbit a) (fromList as))
          y1 = filter (uncurry f) (product (singleOrbit a) (singleOrbit a))
          y2 = map (\(a1, a2) -> (a2, (n, support a1 `intersect` support a2))) (y1 <> y0)
          m0 = Map.fromListWith (\(n1, s1) (n2, s2) -> (n1, s1 `intersect` s2)) . toList $ y2
          clas = Map.keysSet m0
          l0 = head . fromList . fmap snd $ Map.toList m0
      in if a `Set.member` (Map.keysSet phi)
         then go n phi acc as
         else go (n+1) (phi <> m0) ((l0, clas) `cons` acc) as
