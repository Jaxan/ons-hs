module SpecSet (setTests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Prelude (id, not, ($))

import EquivariantSet
import Nominal (atom)
import SpecUtils

setTests :: TestTree
setTests = testGroup "Set" [unitTests]

unitTests :: TestTree
unitTests = testCase "Examples" $ do
  let
    p = atom 1
    q = atom 2
    s = product (singleOrbit (p, q)) (singleOrbit (q, p))

  assert id $ member ((atom 1, atom 2), (atom 5, atom 4)) s
  assert not $ member ((atom 5, atom 2), (atom 5, atom 4)) s
  assert id $ member ((atom 1, atom 2), (atom 2, atom 1)) s
  assert id $ member ((atom 3, atom 4), (atom 2, atom 1)) s

  let s2 = product s s
  assert id $ member (((atom 1, atom 2), (atom 5, atom 4)), ((atom 1, atom 2), (atom 5, atom 4))) s2
  assert id $ member (((atom 1, atom 2), (atom 5, atom 4)), ((atom 1, atom 2), (atom 5, atom 1))) s2
  assert id $ member (((atom 1, atom 2), (atom 5, atom 4)), ((atom 1, atom 200), (atom 5, atom 1))) s2
  assert id $ member (((atom 0, atom 27), (atom 5, atom 4)), ((atom 1, atom 200), (atom 5, atom 1))) s2
  assert not $ member (((atom 0, atom 27), (atom 5, atom 4)), ((atom 1, atom 200), (atom 5, atom 5))) s2

  let s3 = map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
  assert id $ member (((atom 5, atom 4), (atom 1, atom 2)), ((atom 5, atom 4), (atom 1, atom 2))) s3
  assert id $ member (((atom 2, atom 1), (atom 4, atom 5)), ((atom 2, atom 1), (atom 4, atom 5))) s3

  let
    r = atom 3
    s4 = singleOrbit ((p, p), p) `union` singleOrbit ((p, p), q) `union` singleOrbit ((p, q), r)
    s5 = singleOrbit (p, q) `union` singleOrbit (q, r) `union` singleOrbit (r, p)

  assert id $ s5 `isSubsetOf` product (singleOrbit p) (singleOrbit p)
  assert not $ product (singleOrbit p) (singleOrbit p) `isSubsetOf` s5

  let s6 = product s4 s5
  assert id $ member (((atom 1, atom 1), atom 1), (atom 1, atom 2)) s6
  assert id $ member (((atom 37, atom 37), atom 42), (atom 1, atom 2)) s6
  assert not $ member (((atom 37, atom 31), atom 42), (atom 1, atom 2)) s6
  assert id $ member (((atom 1, atom 2), atom 3), (atom 5, atom 4)) s6
