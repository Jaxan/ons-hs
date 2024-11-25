module Nominal.Products where

import Control.Applicative
import Data.MemoTrie
import Data.Proxy

import Nominal.Class

-- * Enumeration of product types

-- $Products
-- This module exports functions to enumerate all orbits in a product. You
-- would typically not use this module directly, instead you can use the
-- @product@ functions from the data structures, such as "OrbitList" or
-- "EquivariantSet".
--
-- Note that the order in which the orbits are enumerated often makes a
-- difference in performance. Currently, orbits with smaller supports are
-- enumerated first. There is now way to customise this order.

-- | Enumerates all orbits in the cartesian product
-- \( A \times B \)
product :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
product = productG prodStrings

-- | Enumerates all orbits in the separated product:
-- \( A * B = \{ (a, b) \in A \times B \mid supp(a) \cap supp(b) = \emptyset \} \)
separatedProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
separatedProduct = productG sepProdStrings

-- | Enumerates all orbits in the (what I call) "left product"
-- \( A ⫂ B = \{ (a, b) \in A \times B \mid supp(a) \supseteq supp(b) \} \)
leftProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
leftProduct = productG lsupprProdStrings

-- | Enumerates all orbits in the "right product"
-- \( A ⫁ B = \{ (a, b) \in A \times B \mid supp(a) \subseteq supp(b) \} \)
rightProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
rightProduct = productG rsupplProdStrings

-- | Strictly increasing product
-- \( \{ (a,b) \mid \text{all elements in } a < \text{all elements in } b \} \)
increasingProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
increasingProduct = productG incrSepProdStrings

-- | Strictly decreasing product
-- \( \{ (a,b) \mid \text{all elements in } a > \text{all elements in } b \} \)
decreasingProduct :: (Nominal a, Nominal b) => Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
decreasingProduct = productG decrSepProdStrings

-- * Helper functions

-- | General combinator, which takes a way to produces "product strings"
-- depending on the size of the support, and then makes the corresponding
-- orbits.
productG :: (Nominal a, Nominal b) => (Int -> Int -> [[Ordering]]) -> Proxy a -> Proxy b -> Orbit a -> Orbit b -> [Orbit (a, b)]
productG strs pa pb oa ob = OrbPair (OrbRec oa) (OrbRec ob) <$> strs (index pa oa) (index pb ob)

-- * Low-level enumeration of product types

-- Enumerates strings to compute all possible combinations. Here `LT` means the
-- "current" element goes to the left, `EQ` goes to both, and `GT` goes to the
-- right. The elements are processed from small to large.
prodStrings :: Alternative f => Int -> Int -> f [Ordering]
prodStrings = memo2 gen
 where
  gen 0 0 = pure []
  gen n 0 = pure $ replicate n LT
  gen 0 n = pure $ replicate n GT
  gen 1 1 = pure [EQ] <|> pure [LT, GT] <|> pure [GT, LT]
  gen n m =
    (EQ :) <$> prodStrings (n - 1) (m - 1)
      <|> (LT :) <$> prodStrings (n - 1) m
      <|> (GT :) <$> prodStrings n (m - 1)

-- Only produces the combinations where the supports are disjoint
sepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
sepProdStrings = memo2 gen
 where
  gen 0 0 = pure []
  gen n 0 = pure $ replicate n LT
  gen 0 n = pure $ replicate n GT
  gen 1 1 = pure [LT, GT] <|> pure [GT, LT]
  gen n m =
    (LT :) <$> sepProdStrings (n - 1) m
      <|> (GT :) <$> sepProdStrings n (m - 1)

-- Combinations where the left element supports the right element
lsupprProdStrings :: Alternative f => Int -> Int -> f [Ordering]
lsupprProdStrings = memo2 gen
 where
  gen n 0 = pure $ replicate n LT
  gen 1 1 = pure [EQ]
  gen n m
    | n < m = empty
    | otherwise =
        (EQ :) <$> lsupprProdStrings (n - 1) (m - 1)
          <|> (LT :) <$> lsupprProdStrings (n - 1) m

-- Combinations where the right element supports the left element
rsupplProdStrings :: Alternative f => Int -> Int -> f [Ordering]
rsupplProdStrings = memo2 gen
 where
  gen 0 n = pure $ replicate n GT
  gen 1 1 = pure [EQ]
  gen n m
    | m < n = empty
    | otherwise =
        (EQ :) <$> rsupplProdStrings (n - 1) (m - 1)
          <|> (GT :) <$> rsupplProdStrings n (m - 1)

-- The right support is strictly greater (hence separated) from the left
incrSepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
incrSepProdStrings = memo2 gen
 where
  gen n m = pure $ replicate n LT <|> replicate m GT

-- The right support is strictly smaller (hence separated) from the left
decrSepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
decrSepProdStrings = memo2 gen
 where
  gen n m = pure $ replicate m GT <|> replicate n LT

{- NOTE on performance:
Previously, I had INLINABLE and SPECIALIZE pragmas for all above definitions.
But with benchmarking, I concluded that they do not make any difference. So
I have removed them. The memoisation does seem to help. So that stays.

I have also tried to enumerate with @Seq a@ instead of @[a]@, but that was
slower. Other choices might be more efficient, I don't know.
-}
