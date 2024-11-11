{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-orphans #-}

import Control.DeepSeq
import Test.Tasty.Bench

import EquivariantMap
import EquivariantSet
import Nominal
import Nominal.Atom
import OrbitList (repeatRationals, size)

instance NFData Atom where
  rnf = rwhnf . unAtom

(\/) :: Ord (Orbit a) => EquivariantSet a -> EquivariantSet a -> EquivariantSet a
(\/) = EquivariantSet.union

bigset :: (Atom, Atom, Atom, _) -> Bool
bigset (p, q, r, t) = EquivariantSet.member t s
 where
  s1 = singleOrbit ((p, p), p) \/ singleOrbit ((p, p), q) \/ singleOrbit ((p, q), r)
  s2 = singleOrbit (p, q) \/ singleOrbit (q, r) \/ singleOrbit (r, p)
  s = EquivariantSet.product s1 s2

bigmap :: (Atom, Atom, _) -> Maybe (Atom, (Atom, Atom))
bigmap (p, q, t) = EquivariantMap.lookup t m3
 where
  s = EquivariantSet.product (EquivariantSet.singleOrbit (p, q)) (EquivariantSet.singleOrbit (q, p))
  s2 = EquivariantSet.product s s
  s3 = EquivariantSet.map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
  m1 = EquivariantMap.fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s2
  m2 = EquivariantMap.fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s3
  m3 = EquivariantMap.unionWith const m1 m2

main :: IO ()
main =
  defaultMain
    [ bgroup
        "bigmap"
        [ bench "1 y" $ nf bigmap (atom 1, atom 2, (((atom 1, atom 23), (atom 5, atom 4)), ((atom 2, atom 3), (atom 54, atom 43)))) -- found
        , bench "2 n" $ nf bigmap (atom 1, atom 2, (((atom 1, atom 23), (atom 5, atom 4)), ((atom 2, atom 3), (atom 54, atom 65)))) -- not found
        , bench "3 y" $ nf bigmap (atom 1, atom 2, (((atom 1, atom 100), (atom 90, atom 20)), ((atom 30, atom 80), (atom 70, atom 65)))) -- found
        , bench "4 y" $ nf bigmap (atom 1, atom 2, (((atom 1, atom 100), (atom 100, atom 1)), ((atom 1, atom 100), (atom 100, atom 1)))) -- found
        , bench "5 y" $ nf bigmap (atom 1, atom 2, (((atom 100, atom 1), (atom 1, atom 100)), ((atom 200, atom 2), (atom 2, atom 200)))) -- found
        ]
    , bgroup
        "bigset"
        [ bench "1 y" $ nf bigset (atom 1, atom 2, atom 3, (((atom 1, atom 1), atom 1), (atom 1, atom 2))) -- found
        , bench "2 y" $ nf bigset (atom 1, atom 2, atom 3, (((atom 37, atom 37), atom 42), (atom 1, atom 2))) -- found
        , bench "3 n" $ nf bigset (atom 1, atom 2, atom 3, (((atom 37, atom 31), atom 42), (atom 1, atom 2))) -- not found
        , bench "4 y" $ nf bigset (atom 1, atom 2, atom 3, (((atom 1, atom 2), atom 3), (atom 5, atom 4))) -- found
        ]
    , bgroup
        "counting orbits"
        [bench (show i) $ nf (sum . OrbitList.size . repeatRationals) i | i <- [1 .. 7]]
    ]
