{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.DeepSeq
import Criterion.Main

import Orbit
import Support
import EquivariantSet
import EquivariantMap

instance NFData Rat

(\/) :: Ord (Orb a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
(\/) = EquivariantSet.union

bigset :: (Rat, Rat, Rat, _) -> Bool
bigset (p, q, r, t) = EquivariantSet.member t s where
  s1 = singleOrbit ((p, p), p) \/ singleOrbit ((p, p), q) \/ singleOrbit ((p, q), r)
  s2 = singleOrbit (p, q) \/ singleOrbit (q, r) \/ singleOrbit (r, p)
  s  = EquivariantSet.product s1 s2

bigmap :: (Rat, Rat, _) -> Maybe (Rat, (Rat, Rat))
bigmap (p, q, t) = EquivariantMap.lookup t m3 where
  s = EquivariantSet.product (EquivariantSet.singleOrbit (p, q)) (EquivariantSet.singleOrbit (q, p))
  s2 = EquivariantSet.product s s
  s3 = EquivariantSet.map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
  m1 = EquivariantMap.fromSet (\(((a, b), (c, d)), ((e, f), (g, h))) -> (b,(d,h))) s2
  m2 = EquivariantMap.fromSet (\(((a, b), (c, d)), ((e, f), (g, h))) -> (b,(d,h))) s3
  m3 = EquivariantMap.unionWith const m1 m2

main :: IO ()
main = defaultMain
         -- ~ 300 ms
         [ bgroup "bigmap"
             [ bench "1 y" $ nf bigmap (Rat 1, Rat 2, (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 43)))) -- found
             , bench "2 n" $ nf bigmap (Rat 1, Rat 2, (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 65)))) -- not found
             , bench "3 y" $ nf bigmap (Rat 1, Rat 2, (((Rat 1, Rat 100), (Rat 90, Rat 20)), ((Rat 30, Rat 80), (Rat 70, Rat 65)))) -- found
             , bench "4 y" $ nf bigmap (Rat 1, Rat 2, (((Rat 1, Rat 100), (Rat 100, Rat 1)), ((Rat 1, Rat 100), (Rat 100, Rat 1)))) -- found
             , bench "5 y" $ nf bigmap (Rat 1, Rat 2, (((Rat 100, Rat 1), (Rat 1, Rat 100)), ((Rat 200, Rat 2), (Rat 2, Rat 200)))) -- found
             ]
         -- ~ 13 us
         , bgroup "bigset"
             [ bench "1 y" $ nf bigset (Rat 1, Rat 2, Rat 3, ( ((Rat 1, Rat 1), Rat 1), (Rat 1, Rat 2) )) -- found
             , bench "2 y" $ nf bigset (Rat 1, Rat 2, Rat 3, ( ((Rat 37, Rat 37), Rat 42), (Rat 1, Rat 2) )) -- found
             , bench "3 n" $ nf bigset (Rat 1, Rat 2, Rat 3, ( ((Rat 37, Rat 31), Rat 42), (Rat 1, Rat 2) )) -- not found
             , bench "4 y" $ nf bigset (Rat 1, Rat 2, Rat 3, ( ((Rat 1, Rat 2), Rat 3), (Rat 5, Rat 4) )) -- found
             ]
         ]
