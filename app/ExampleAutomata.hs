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

-- All example automata follow below

-- words of length <= m
words m = fold $ go (m+1) (singleOrbit []) where
  go 0 _   = []
  go k acc = acc : go (k-1) (productWith (:) rationals acc)

fromKeys f = Map.fromSet f . Set.fromOrbitList


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
data Fifo = Put Atom | Get Atom
  deriving (Eq, Ord, Show, GHC.Generic)
  deriving Nominal via Generic Fifo

instance ToStr Fifo where
  toStr (Put a) = "Put " ++ toStr a
  toStr (Get a) = "Get " ++ toStr a

instance FromStr Fifo where
  fromStr ('P':'u':'t':' ':a) = let (x, r) = fromStr a in (Put x, r)
  fromStr ('G':'e':'t':' ':a) = let (x, r) = fromStr a in (Get x, r)
  fromStr _ = error "Cannot parse Fifo"

data FifoS = FifoS [Atom] [Atom]
  deriving (Eq, Ord, GHC.Generic)
  deriving Nominal via Generic FifoS

instance ToStr FifoS where
  toStr (FifoS l1 l2) = "F " ++ toStr l1 ++ " - " ++ toStr l2

fifoAlph = map Put rationals <> map Get rationals

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
