{-# OPTIONS_GHC -Wno-orphans #-}

module SpecUtils where

import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck as QC

import Nominal.Atom

assert :: HasCallStack => (a -> Bool) -> a -> IO ()
assert f x = assertBool "" (f x)

instance Arbitrary Atom where
  arbitrary = atom <$> arbitrary
  shrink (Atom p) = atom <$> shrink p
