{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-orphans #-}

import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck as QC
import Prelude (Bool (..), Eq (..), IO, Int, const, id, length, not, show, (!!), ($), (<$>))

import EquivariantMap (fromSet, lookup, unionWith)
import EquivariantSet (isSubsetOf, map, member, product, singleOrbit, union)
import Nominal (Nominal (..))
import OrbitList (repeatRationals, size)
import Support (Rat (..))

assert :: HasCallStack => (a -> Bool) -> a -> IO ()
assert f x = assertBool "" (f x)

main :: IO ()
main = defaultMain (testGroup "main" [unitTests, countingTests, qcTests])


unitTests :: _
unitTests = testCase "Examples" $ do
  let p  = Rat 1
  let q  = Rat 2
  let s  = product (singleOrbit (p, q)) (singleOrbit (q, p))
  assert id  $ member ((Rat 1, Rat 2), (Rat 5, Rat 4)) s
  assert not $ member ((Rat 5, Rat 2), (Rat 5, Rat 4)) s
  assert id  $ member ((Rat 1, Rat 2), (Rat 2, Rat 1)) s
  assert id  $ member ((Rat 3, Rat 4), (Rat 2, Rat 1)) s

  let s2 = product s s
  assert id  $ member (((Rat 1, Rat 2), (Rat 5, Rat 4)), ((Rat 1, Rat 2), (Rat 5, Rat 4))) s2
  assert id  $ member (((Rat 1, Rat 2), (Rat 5, Rat 4)), ((Rat 1, Rat 2), (Rat 5, Rat 1))) s2
  assert id  $ member (((Rat 1, Rat 2), (Rat 5, Rat 4)), ((Rat 1, Rat 200), (Rat 5, Rat 1))) s2
  assert id  $ member (((Rat 0, Rat 27), (Rat 5, Rat 4)), ((Rat 1, Rat 200), (Rat 5, Rat 1))) s2
  assert not $ member (((Rat 0, Rat 27), (Rat 5, Rat 4)), ((Rat 1, Rat 200), (Rat 5, Rat 5))) s2

  let s3 = map (\((a, b), (c, d)) -> ((b, a), (d, c))) s2
  assert id  $ member (((Rat 5, Rat 4), (Rat 1, Rat 2)), ((Rat 5, Rat 4), (Rat 1, Rat 2))) s3
  assert id  $ member (((Rat 2, Rat 1), (Rat 4, Rat 5)), ((Rat 2, Rat 1), (Rat 4, Rat 5))) s3

  let m1 = fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s2
  assert isJust $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 3, Rat 2))) m1
  assert isNothing $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 1, Rat 2))) m1

  let m2 = fromSet (\(((_, b), (_, d)), (_, (_, h))) -> (b, (d, h))) s3
  assert isJust $ lookup (((Rat 6, Rat 1), (Rat 1, Rat 5)), ((Rat 4, Rat 1), (Rat 1, Rat 3))) m2
  assert isNothing $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 4, Rat 2))) m2

  let m3 = unionWith const m1 m2
  assert isJust    $ lookup (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 43))) m3
  assert isNothing $ lookup (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 65))) m3
  assert isJust    $ lookup (((Rat 1, Rat 100), (Rat 90, Rat 20)), ((Rat 30, Rat 80), (Rat 70, Rat 65))) m3
  assert isJust    $ lookup (((Rat 1, Rat 100), (Rat 100, Rat 1)), ((Rat 1, Rat 100), (Rat 100, Rat 1))) m3
  assert isJust    $ lookup (((Rat 100, Rat 1), (Rat 1, Rat 100)), ((Rat 200, Rat 2), (Rat 2, Rat 200))) m3

  let r = Rat 3
  let s4 = singleOrbit ((p, p), p) `union` singleOrbit ((p, p), q) `union` singleOrbit ((p, q), r)
  let s5 = singleOrbit (p, q) `union` singleOrbit (q, r) `union` singleOrbit (r, p)
  assert id $ s5 `isSubsetOf` product (singleOrbit p) (singleOrbit p)
  assert not $ product (singleOrbit p) (singleOrbit p) `isSubsetOf` s5

  let s6 = product s4 s5
  assert id $ member (((Rat 1, Rat 1), Rat 1), (Rat 1, Rat 2)) s6
  assert id $ member (((Rat 37, Rat 37), Rat 42), (Rat 1, Rat 2)) s6
  assert not $ member (((Rat 37, Rat 31), Rat 42), (Rat 1, Rat 2)) s6
  assert id $ member (((Rat 1, Rat 2), Rat 3), (Rat 5, Rat 4)) s6


-- Verifying that the number of orbits is correct. Up to length 7, because
-- length 8 and longer take at least one second.
countingTests :: _
countingTests = testGroup "Counting" [testCase (show n) $ length (OrbitList.size (repeatRationals n)) @?= (a000670 !! n) | n <- [0..7]]

-- A000670: Ordered Bell numbers or Fubini numbers
a000670 :: [Int]
a000670 = [1, 1, 3, 13, 75, 541, 4683, 47293, 545835, 7087261, 102247563, 1622632573, 28091567595]


-- TODO: Add more quickcheck tests
qcTests :: _
qcTests = testGroup "QuickCheck" [QC.testProperty "all atoms in same orbit" $ \p q -> toOrbit (p :: Rat) == toOrbit (q :: Rat)]

instance Arbitrary Rat where
  arbitrary = Rat <$> arbitrary
  shrink (Rat p) = Rat <$> shrink p
