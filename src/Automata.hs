{-# language FlexibleContexts #-}

module Automata where

import Nominal (Nominal(..))
import OrbitList (OrbitList)
import EquivariantMap (EquivariantMap, (!))

import Prelude hiding (Word)


type Word a = [a]

-- states, initial state, acceptance, transition
data Automaton q a = Automaton
  { states :: OrbitList q
  , initialState :: q
  , acceptance :: EquivariantMap q Bool
  , transition :: EquivariantMap (q, a) q
  }

accepts :: (Nominal q, Ord (Orbit q), Nominal a, Ord (Orbit a))
        => Automaton q a -> Word a -> Bool
accepts aut l = go (initialState aut) l
  where
    go s []    = acceptance aut ! s
    go s (a:w) = go (transition aut ! (s, a)) w
