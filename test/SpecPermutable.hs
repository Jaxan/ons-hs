{-# OPTIONS_GHC -Wno-orphans #-}

module SpecPermutable (permutableTests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

import Nominal
import Permutable
import Support (Rat (..))

import SpecUtils

permutableTests :: TestTree
permutableTests = testGroup "Permutable" [assocTest n | n <- [0 .. 6]]

-- For n = 7, this takes roughly 30 seconds!
assocTest :: Int -> TestTree
assocTest n =
  testCase ("associativity " <> show n) $
    assert and $
      [lhs f g == rhs f g | f <- perms, g <- perms]
 where
  element = fmap (Rat . toRational) $ [1 .. n]
  supp = support element
  perms = allPermutations supp
  lhs f g = act (Permuted (compose f g) element)
  rhs f g = act (Permuted f (act (Permuted g element)))
