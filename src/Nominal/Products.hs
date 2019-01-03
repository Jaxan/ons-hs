module Nominal.Products where

import Control.Applicative
import Data.MemoTrie

prodStrings :: Alternative f => Int -> Int -> f [Ordering]
prodStrings = memo2 gen where
  gen 0 0 = pure []
  gen 0 n = pure $ replicate n GT
  gen n 0 = pure $ replicate n LT
  gen 1 1 = pure [LT, GT] <|> pure [EQ] <|> pure [GT, LT]
  gen n m = (LT :) <$> prodStrings (n-1) m
        <|> (EQ :) <$> prodStrings (n-1) (m-1)
        <|> (GT :) <$> prodStrings n (m-1)

sepProdStrings :: Alternative f => Int -> Int -> f [Ordering]
sepProdStrings = memo2 gen where
  gen 0 0 = pure []
  gen 0 n = pure $ replicate n GT
  gen n 0 = pure $ replicate n LT
  gen 1 1 = pure [LT, GT] <|> pure [GT, LT]
  gen n m = (LT :) <$> sepProdStrings (n-1) m
        <|> (GT :) <$> sepProdStrings n (m-1)

rincProdStrings :: Alternative f => Int -> Int -> f [Ordering]
rincProdStrings = memo2 gen where
  gen n 0 = pure $ replicate n LT
  gen 0 _ = empty
  gen 1 1 = pure [EQ]
  gen n m 
    | n < m     = empty
    | otherwise = (LT :) <$> rincProdStrings (n-1) m
              <|> (EQ :) <$> rincProdStrings (n-1) (m-1)

{-# INLINABLE prodStrings #-}
{-# INLINABLE sepProdStrings #-}
{-# INLINABLE rincProdStrings #-}

{-# SPECIALIZE prodStrings :: Int -> Int -> [[Ordering]] #-}
{-# SPECIALIZE sepProdStrings :: Int -> Int -> [[Ordering]] #-}
{-# SPECIALIZE rincProdStrings :: Int -> Int -> [[Ordering]] #-}
