module SpecSet (setTests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Prelude (id, not, ($))

import EquivariantSet
import Support (Rat (..))

import SpecUtils

setTests :: TestTree
setTests = testGroup "Set" [unitTests]

unitTests :: TestTree
unitTests = testCase "Examples" $ do
  let
    p = Rat 1
    q = Rat 2
    s = product (singleOrbit (p, q)) (singleOrbit (q, p))

  assert id $ member ((Rat 1, Rat 2), (Rat 5, Rat 4)) s
  assert not $ member ((Rat 5, Rat 2), (Rat 5, Rat 4)) s
  assert id $ member ((Rat 1, Rat 2), (Rat 2, Rat 1)) s
  assert id $ member ((Rat 3, Rat 4), (Rat 2, Rat 1)) s

  let s2 = product s s
  assert id $ member (((Rat 1, Rat 2), (Rat 5, Rat 4)), ((Rat 1, Rat 2), (Rat 5, Rat 4))) s2
  assert id $ member (((Rat 1, Rat 2), (Rat 5, Rat 4)), ((Rat 1, Rat 2), (Rat 5, Rat 1))) s2
  assert id $ member (((Rat 1, Rat 2), (Rat 5, Rat 4)), ((Rat 1, Rat 200), (Rat 5, Rat 1))) s2
  assert id $ member (((Rat 0, Rat 27), (Rat 5, Rat 4)), ((Rat 1, Rat 200), (Rat 5, Rat 1))) s2
  assert not $ member (((Rat 0, Rat 27), (Rat 5, Rat 4)), ((Rat 1, Rat 200), (Rat 5, Rat 5))) s2

  let s3 = map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
  assert id $ member (((Rat 5, Rat 4), (Rat 1, Rat 2)), ((Rat 5, Rat 4), (Rat 1, Rat 2))) s3
  assert id $ member (((Rat 2, Rat 1), (Rat 4, Rat 5)), ((Rat 2, Rat 1), (Rat 4, Rat 5))) s3

  let
    r = Rat 3
    s4 = singleOrbit ((p, p), p) `union` singleOrbit ((p, p), q) `union` singleOrbit ((p, q), r)
    s5 = singleOrbit (p, q) `union` singleOrbit (q, r) `union` singleOrbit (r, p)

  assert id $ s5 `isSubsetOf` product (singleOrbit p) (singleOrbit p)
  assert not $ product (singleOrbit p) (singleOrbit p) `isSubsetOf` s5

  let s6 = product s4 s5
  assert id $ member (((Rat 1, Rat 1), Rat 1), (Rat 1, Rat 2)) s6
  assert id $ member (((Rat 37, Rat 37), Rat 42), (Rat 1, Rat 2)) s6
  assert not $ member (((Rat 37, Rat 31), Rat 42), (Rat 1, Rat 2)) s6
  assert id $ member (((Rat 1, Rat 2), Rat 3), (Rat 5, Rat 4)) s6
