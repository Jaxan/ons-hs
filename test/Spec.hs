{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- TODO: QuickCheck instead of these unit tests

import GHC.Stack

import Data.Maybe (isJust, isNothing)
import Prelude (id, const, not, ($), error, return, Bool(..), IO(..), print, (>>=))
import qualified Prelude as P -- hide stuff

import Support (Rat(..))
import EquivariantSet (product, member, singleOrbit, union, map, isSubsetOf)
import EquivariantMap (unionWith, lookup, fromSet)

assert :: (a -> Bool) -> a -> IO ()
assert f x = case f x of
  True -> return ()
  False -> whoCreated x >>= print

main :: IO ()
main = do
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

  let m1 = fromSet (\(((a, b), (c, d)), ((e, f), (g, h))) -> (b,(d,h))) s2
  assert isJust    $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 3, Rat 2))) m1
  assert isNothing $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 1, Rat 2))) m1

  let m2 = fromSet (\(((a, b), (c, d)), ((e, f), (g, h))) -> (b,(d,h))) s3
  assert isJust    $ lookup (((Rat 6, Rat 1), (Rat 1, Rat 5)), ((Rat 4, Rat 1), (Rat 1, Rat 3))) m2
  assert isNothing $ lookup (((Rat 1, Rat 2), (Rat 2, Rat 1)), ((Rat 1, Rat 2), (Rat 4, Rat 2))) m2

  let m3 = unionWith const m1 m2
  assert isJust    $ lookup (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 43))) m3
  assert isNothing $ lookup (((Rat 1, Rat 23), (Rat 5, Rat 4)), ((Rat 2, Rat 3), (Rat 54, Rat 65))) m3
  assert isJust    $ lookup (((Rat 1, Rat 100), (Rat 90, Rat 20)), ((Rat 30, Rat 80), (Rat 70, Rat 65))) m3
  assert isJust    $ lookup (((Rat 1, Rat 100), (Rat 100, Rat 1)), ((Rat 1, Rat 100), (Rat 100, Rat 1))) m3
  assert isJust    $ lookup (((Rat 100, Rat 1), (Rat 1, Rat 100)), ((Rat 200, Rat 2), (Rat 2, Rat 200))) m3

  let r = Rat 3
  let s1 = singleOrbit ((p, p), p) `union` singleOrbit ((p, p), q) `union` singleOrbit ((p, q), r)
  let s2 = singleOrbit (p, q) `union` singleOrbit (q, r) `union` singleOrbit (r, p)
  assert id  $ s2 `isSubsetOf` product (singleOrbit p) (singleOrbit p)
  assert not $ product (singleOrbit p) (singleOrbit p) `isSubsetOf` s2

  let s  = product s1 s2
  assert id  $ member ( ((Rat 1, Rat 1), Rat 1), (Rat 1, Rat 2) ) s
  assert id  $ member ( ((Rat 37, Rat 37), Rat 42), (Rat 1, Rat 2) ) s
  assert not $ member ( ((Rat 37, Rat 31), Rat 42), (Rat 1, Rat 2) ) s
  assert id  $ member ( ((Rat 1, Rat 2), Rat 3), (Rat 5, Rat 4) ) s

