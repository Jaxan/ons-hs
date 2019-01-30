{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ExampleAutomata
  ( module ExampleAutomata
  , module Automata
  ) where

import Nominal hiding (product)
import Automata
import IO
import OrbitList
import qualified EquivariantMap as Map
import qualified EquivariantSet as Set

import Data.Foldable (fold)
import qualified GHC.Generics as GHC
import Prelude as P hiding (map, product, words, filter, foldr)


atoms = rationals

-- words of length <= m
words m = fold $ go (m+1) (singleOrbit []) where
  go 0 _   = []
  go k acc = acc : go (k-1) (productWith (:) atoms acc)

fromKeys f = Map.fromSet f . Set.fromOrbitList

ltPair = filter (\(a, b) -> a < b) $ product atoms atoms


data DoubleWord = Store [Atom] | Check [Atom] | Accept | Reject
  deriving (Eq, Ord, GHC.Generic)
  deriving Nominal via Generic DoubleWord

instance ToStr DoubleWord where
  toStr (Store w) = "S " ++ toStr w
  toStr (Check w) = "C " ++ toStr w
  toStr Accept    = "A"
  toStr Reject    = "R"

doubleWordAut 0 = Automaton {..} where
  states = fromList [Accept, Reject]
  initialState = Accept
  acceptance = fromKeys (Accept ==) states
  transition = fromKeys (const Reject) $ product states rationals
doubleWordAut n = Automaton {..} where
  states = fromList [Accept, Reject] <> map Store (words (n-1)) <> map Check (productWith (:) rationals (words (n-1)))
  initialState = Store []
  acceptance = fromKeys (Accept ==) states
  trans Accept _ = Reject
  trans Reject _ = Reject
  trans (Store l) a
    | length l + 1 < n = Store (a:l)
    | otherwise        = Check (reverse (a:l))
  trans (Check (a:as)) b
    | a == b    = if (P.null as) then Accept else Check as
    | otherwise = Reject
  transition = fromKeys (uncurry trans) $ product states rationals


-- alphetbet for the Fifo queue example
data FifoA = Put Atom | Get Atom
  deriving (Eq, Ord, Show, GHC.Generic)
  deriving Nominal via Generic FifoA

instance ToStr FifoA where
  toStr (Put a) = "Put " ++ toStr a
  toStr (Get a) = "Get " ++ toStr a

instance FromStr FifoA where
  fromStr ('P':'u':'t':' ':a) = let (x, r) = fromStr a in (Put x, r)
  fromStr ('G':'e':'t':' ':a) = let (x, r) = fromStr a in (Get x, r)
  fromStr _ = error "Cannot parse Fifo"

fifoAlph = map Put rationals <> map Get rationals

data FifoS = FifoS [Atom] [Atom]
  deriving (Eq, Ord, GHC.Generic)
  deriving Nominal via Generic FifoS

instance ToStr FifoS where
  toStr (FifoS l1 l2) = "F " ++ toStr l1 ++ " - " ++ toStr l2

fifoAut n = Automaton {..} where
  states0 = filter (\(FifoS l1 l2) -> length l1 + length l2 <= n) $ productWith (\l1 l2 -> FifoS l1 l2) (words n) (words n)
  states = fromList [Nothing] <> map Just states0
  initialState = Just (FifoS [] [])
  acceptance = fromKeys (Nothing /=) states
  trans Nothing _ = Nothing
  trans (Just (FifoS l1 l2)) (Put a)
    | length l1 + length l2 >= n = Nothing
    | otherwise                  = Just (FifoS (a:l1) l2)
  trans (Just (FifoS [] [])) (Get _) = Nothing
  trans (Just (FifoS l1 [])) (Get b) = trans (Just (FifoS [] (reverse l1))) (Get b)
  trans (Just (FifoS l1 (a:l2))) (Get b)
    | a == b    = Just (FifoS l1 l2)
    | otherwise = Nothing
  transition = fromKeys (uncurry trans) $ product states fifoAlph


data Lint a = Lint_start | Lint_single a | Lint_full a a | Lint_semi a a | Lint_error
  deriving (Eq, Ord, Show, GHC.Generic)
  deriving Nominal via Generic (Lint a)

lintExample ::Automaton (Lint Atom) Atom
lintExample = Automaton {..} where
    states = fromList [Lint_start, Lint_error]
        <> map Lint_single atoms
        <> map (uncurry Lint_full) ltPair
        <> map (uncurry Lint_semi) ltPair
    initialState = Lint_start
    acc (Lint_full _ _) = True
    acc _ = False
    acceptance = fromKeys acc states
    trans Lint_start a = Lint_single a
    trans (Lint_single a) b
      | a < b = Lint_full a b
      | otherwise = Lint_error
    trans (Lint_full a b) c
      | a < c && c < b = Lint_semi c b
      | otherwise = Lint_error
    trans (Lint_semi a b) c
      | a < c && c < b = Lint_full a c
      | otherwise = Lint_error
    trans Lint_error _ = Lint_error
    transition = fromKeys (uncurry trans) $ product states atoms


data Lmax a = Lmax_start | Lmax_single a | Lmax_double a a
  deriving (Eq, Ord, Show, GHC.Generic)
  deriving Nominal via Generic (Lmax a)

lmaxExample :: Automaton (Lmax Atom) Atom
lmaxExample = Automaton {..} where
    states = singleOrbit Lmax_start
        <> map Lmax_single atoms
        <> productWith Lmax_double atoms atoms
    initialState = Lmax_start
    acc (Lmax_double a b) = a == b
    acc _ = False
    acceptance = fromKeys acc states
    trans Lmax_start a = Lmax_single a
    trans (Lmax_single a) b = Lmax_double a b
    trans (Lmax_double b c) a
      | b >= c = Lmax_double b a
      | otherwise = Lmax_double c a
    transition = fromKeys (uncurry trans) $ product states atoms


