module Nominal.Products where

import Control.Applicative
import Data.MemoTrie

-- Enumerates strings to compute all possible combinations. Here `LT` means the
-- "current" element goes to the left, `EQ` goes to both, and `GT` goes to the
-- right. The elements are processed from small to large.
prodStrings :: Alternative f => Int -> Int -> f [Ordering]
prodStrings = memo2 gen where
  gen 0 0 = pure []
  gen n 0 = pure $ replicate n LT
  gen 0 n = pure $ replicate n GT
  gen 1 1 = pure [LT, GT] <|> pure [EQ] <|> pure [GT, LT]
  gen n m = (LT :) <$> prodStrings (n-1) m
        <|> (EQ :) <$> prodStrings (n-1) (m-1)
        <|> (GT :) <$> prodStrings n (m-1)

-- Only produces the combinations where the supports are disjoint
sepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
sepProdStrings = memo2 gen where
  gen 0 0 = pure []
  gen n 0 = pure $ replicate n LT
  gen 0 n = pure $ replicate n GT
  gen 1 1 = pure [LT, GT] <|> pure [GT, LT]
  gen n m = (LT :) <$> sepProdStrings (n-1) m
        <|> (GT :) <$> sepProdStrings n (m-1)

-- Combinations where the left element supports the right element
lsupprProdStrings :: Alternative f => Int -> Int -> f [Ordering]
lsupprProdStrings = memo2 gen where
  gen n 0 = pure $ replicate n LT
  gen 1 1 = pure [EQ]
  gen n m
    | n < m     = empty
    | otherwise = (LT :) <$> lsupprProdStrings (n-1) m
              <|> (EQ :) <$> lsupprProdStrings (n-1) (m-1)

-- Combinations where the right element supports the left element
rsupplProdStrings :: Alternative f => Int -> Int -> f [Ordering]
rsupplProdStrings = memo2 gen where
  gen 0 n = pure $ replicate n GT
  gen 1 1 = pure [EQ]
  gen n m
    | m < n     = empty
    | otherwise = (EQ :) <$> rsupplProdStrings (n-1) (m-1)
              <|> (GT :) <$> rsupplProdStrings n (m-1)

-- The right support is strictly greater (hence separated) from the left
incrSepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
incrSepProdStrings = memo2 gen where
  gen n m = pure $ replicate n LT <|> replicate m GT

-- The right support is strictly smaller (hence separated) from the left
decrSepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
decrSepProdStrings = memo2 gen where
  gen n m = pure $ replicate m GT <|> replicate n LT

testProdStrings :: Alternative f => Int -> Int -> f [Ordering]
testProdStrings = mgen (0 :: Int) where
  mgen = memo3 gen
  gen _ n 0 = pure $ replicate n LT
  gen _ 0 n = pure $ replicate n GT
  gen 0 n m = (LT :) <$> mgen 1 (n-1) m
          <|> (EQ :) <$> mgen 0 (n-1) (m-1)
  gen k n m = (LT :) <$> mgen (k+1) (n-1) m
          <|> (EQ :) <$> mgen k (n-1) (m-1)
          <|> (GT :) <$> mgen (k-1) n (m-1)


{- NOTE on performance:
Previously, I had INLINABLE and SPECIALIZE pragmas for all above definitions.
But with benchmarking, I concluded that they do not make any difference. So
I have removed them. The memoisation does seem to help. So that stays.
-}
