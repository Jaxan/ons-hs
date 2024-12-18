{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language PartialTypeSignatures #-}
{-# language RecordWildCards #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import EquivariantMap ((!))
import ExampleAutomata
import FileAutomata
import IO
import OrbitList
import Quotient
import qualified EquivariantMap as Map
import qualified EquivariantSet as Set

import Prelude as P hiding (map, product, words, filter, foldr)
import System.Environment
import System.IO


-- Version A: works on equivalence relations
minimiseA :: _ => OrbitList a -> Automaton q a -> Automaton _ a
minimiseA alph Automaton{..} = Automaton
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
minimiseB :: _ => OrbitList a -> Automaton q a -> Automaton _ a
minimiseB alph Automaton{..} = Automaton
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
  f:w <- getArgs
  case f of
    "Lmax" -> putStrLn . toStr . minimiseB rationals $ lmaxExample
    "Lint" -> putStrLn . toStr . minimiseB rationals $ lintExample
    "Fifo" -> putStrLn . toStr . minimiseB fifoAlph $ fifoAut (read (P.head w) :: Int)
    "DoubleWord" -> putStrLn . toStr . minimiseB rationals $ doubleWordAut (read (P.head w) :: Int)
    "File" -> do
      let m f = fileAutomaton f >>= \(aut, alph) -> putStrLn . toStr . minimiseB alph $ aut
      mapM_ m w
    "Formula" -> do
      (aut, alph) <- formulaAutomaton (P.head w)
      putStrLn . toStr . minimiseB alph $ aut

