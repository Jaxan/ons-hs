{-# LANGUAGE ImportQualifiedPost #-}

module SpecMap (mapTests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Prelude (const, ($))

import EquivariantMap
import EquivariantSet qualified as EqSet
import Support

import SpecUtils

mapTests :: TestTree
mapTests = testGroup "Map" [unitTests]

unitTests :: TestTree
unitTests = testCase "Examples" $ do
  let
    p = Rat 1
    q = Rat 2
    s = EqSet.product (EqSet.singleOrbit (p, q)) (EqSet.singleOrbit (q, p))
    s2 = EqSet.product s s
    m1 = fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s2

  assert isJust $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 3, Rat 2))) m1
  assert isNothing $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 1, Rat 2))) m1

  let
    s3 = EqSet.map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
    m2 = fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s3

  assert isJust $ lookup (((Rat 6, Rat 1), (Rat 1, Rat 5)), ((Rat 4, Rat 1), (Rat 1, Rat 3))) m2
  assert isNothing $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 4, Rat 2))) m2

  let m3 = unionWith const m1 m2
  assert isJust $ lookup (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 43))) m3
  assert isNothing $ lookup (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 65))) m3
  assert isJust $ lookup (((Rat 1, Rat 100), (Rat 90, Rat 20)), ((Rat 30, Rat 80), (Rat 70, Rat 65))) m3
  assert isJust $ lookup (((Rat 1, Rat 100), (Rat 100, Rat 1)), ((Rat 1, Rat 100), (Rat 100, Rat 1))) m3
  assert isJust $ lookup (((Rat 100, Rat 1), (Rat 1, Rat 100)), ((Rat 200, Rat 2), (Rat 2, Rat 200))) m3
