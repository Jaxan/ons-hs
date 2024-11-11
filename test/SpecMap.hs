{-# LANGUAGE ImportQualifiedPost #-}

module SpecMap (mapTests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Prelude (const, ($))

import EquivariantMap
import EquivariantSet qualified as EqSet
import Nominal (atom)
import SpecUtils

mapTests :: TestTree
mapTests = testGroup "Map" [unitTests]

unitTests :: TestTree
unitTests = testCase "Examples" $ do
  let
    p = atom 1
    q = atom 2
    s = EqSet.product (EqSet.singleOrbit (p, q)) (EqSet.singleOrbit (q, p))
    s2 = EqSet.product s s
    m1 = fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s2

  assert isJust $ lookup (((atom 1, atom 2), (atom 2, atom 1)), ((atom 1, atom 2), (atom 3, atom 2))) m1
  assert isNothing $ lookup (((atom 1, atom 2), (atom 2, atom 1)), ((atom 1, atom 2), (atom 1, atom 2))) m1

  let
    s3 = EqSet.map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
    m2 = fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s3

  assert isJust $ lookup (((atom 6, atom 1), (atom 1, atom 5)), ((atom 4, atom 1), (atom 1, atom 3))) m2
  assert isNothing $ lookup (((atom 1, atom 2), (atom 2, atom 1)), ((atom 1, atom 2), (atom 4, atom 2))) m2

  let m3 = unionWith const m1 m2
  assert isJust $ lookup (((atom 1, atom 23), (atom 5, atom 4)), ((atom 2, atom 3), (atom 54, atom 43))) m3
  assert isNothing $ lookup (((atom 1, atom 23), (atom 5, atom 4)), ((atom 2, atom 3), (atom 54, atom 65))) m3
  assert isJust $ lookup (((atom 1, atom 100), (atom 90, atom 20)), ((atom 30, atom 80), (atom 70, atom 65))) m3
  assert isJust $ lookup (((atom 1, atom 100), (atom 100, atom 1)), ((atom 1, atom 100), (atom 100, atom 1))) m3
  assert isJust $ lookup (((atom 100, atom 1), (atom 1, atom 100)), ((atom 200, atom 2), (atom 2, atom 200))) m3
