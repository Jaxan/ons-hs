{-# OPTIONS_GHC -Wno-orphans #-}

module SpecUtils where

import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck as QC

import Support (Rat (..))

assert :: HasCallStack => (a -> Bool) -> a -> IO ()
assert f x = assertBool "" (f x)

instance Arbitrary Rat where
  arbitrary = Rat <$> arbitrary
  shrink (Rat p) = Rat <$> shrink p
