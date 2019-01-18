{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language PartialTypeSignatures #-}
{-# language RecordWildCards #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Nominal hiding (product)
import Support (Rat(..), Support)
import OnsAutomata
import OnsQuotient
import OrbitList
import EquivariantMap ((!))
import qualified EquivariantMap as Map
import qualified EquivariantSet as Set

import Data.Foldable (fold)
import qualified GHC.Generics as GHC
import Prelude as P hiding (map, product, words, filter, foldr)


-- Version A: works on equivalence relations
minimiseA :: _ => Automaton q a -> OrbitList a -> Automaton _ a
minimiseA Automaton{..} alph = Automaton
  { states = states2
  , initialState = phi ! initialState
  , acceptance = Map.fromList . fmap (\(s, b) -> (phi ! s, b)) . Map.toList $ acceptance
  , transition = Map.fromList . fmap (\((s, a), t) -> ((phi ! s, a), phi ! t)) . Map.toList $ transition
  }
  where
    -- Are all successors of s0 t0 related?
    nextAreEquiv equiv s0 t0 = OrbitList.null
      . filter (\(s2, t2) -> s2 /= t2 && not ((s2, t2) `Set.member` equiv))
      $ productWith (\(s, t) a -> (transition ! (s, a), transition ! (t, a))) (singleOrbit (s0, t0)) alph
    -- Recursively shrink equiv to fixpoint
    go equiv = let (equiv2, nonEq) = Set.partition (\(s, t) -> s == t || nextAreEquiv equiv s t) equiv
               in if Set.null nonEq -- fixpoint!
                  then equiv
                  else go equiv2
    -- Start with FxF + NxN
    (y, n) = partition (acceptance !) states
    equiv0 = Set.fromOrbitList $ y `product` y <> n `product` n
    -- compute fixpoint
    equivInf = go equiv0
    -- get quotient
    (phi, states2) = quotient equivInf states


-- Version B: works on quotient maps
minimiseB :: _ => Automaton q a -> OrbitList a -> Automaton _ a
minimiseB Automaton{..} alph = Automaton
  { states = map fst stInf
  , initialState = phiInf ! initialState
  , acceptance = Map.fromList . fmap (\(s, b) -> (phiInf ! s, b)) . Map.toList $ acceptance
  , transition = Map.fromList . fmap (\((s, a), t) -> ((phiInf ! s, a), phiInf ! t)) . Map.toList $ transition
  }
  where
    -- Are all successors of s0 t0 related?
    nextAreEquiv phi s0 t0 = OrbitList.null
      . filter (\(s2, t2) -> s2 /= t2 && phi ! s2 /= phi ! t2)
      $ productWith (\a (s, t) -> (transition ! (s, a), transition ! (t, a))) alph (singleOrbit (s0, t0))
    -- Are s0 t0 equivalent with current information?
    equiv phi s0 t0 = s0 == t0 || (phi ! s0 == phi ! t0 && nextAreEquiv phi s0 t0)
    addMid p a (f, b, k) = (p <> f, b <> a, k)
    -- Given a quotientmap, refine it, per "leaf"
    go phi st = let (phi2, st2, _) = foldr (\(_, clas) (phix, acc, k) -> addMid phix acc . quotientf k (equiv phi) . Set.toOrbitList $ clas) (mempty, empty, 0) st
             in if size st == size st2 -- fixpoint
                then (phi, st)
                else go phi2 st2
    -- Start with acceptance as quotient map
    (phi0, st0, _) = quotientf 0 (\a b -> a == b || acceptance ! a == acceptance ! b) states
    -- Compute fixpoint
    (phiInf, stInf) = go phi0 st0


main :: IO ()
main = do
  -- putStrLn . toStr $ (doubleWordAut 4)
  putStrLn . toStr $ (minimiseB (doubleWordAut 4) rationals)


-- All example automata follow below

-- words of length <= m
words m = fold $ go (m+1) (singleOrbit []) where
  go 0 acc = []
  go k acc = acc : go (k-1) (productWith (:) rationals acc)

fromKeys f = Map.fromSet f . Set.fromOrbitList


data DoubleWord = Store [Rat] | Check [Rat] | Accept | Reject
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


data FifoS = FifoS [Rat] [Rat]
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
