{-# LANGUAGE FlexibleContexts #-}

module Main where

import Nominal hiding (product)
import Support (Rat(..))
import OrbitList
import EquivariantMap (EquivariantMap, lookup, fromSet)
import EquivariantSet (fromOrbitList, toList)

import Prelude hiding (filter, null, elem, lookup, product, Word, map)

type Word a    = [a]
type Alph a    = OrbitList a
type Rows a    = OrbitList (Word a)
type Columns a = OrbitList (Word a)
type Table a   = EquivariantMap (Word a, Word a) Bool

unequalRows :: (Nominal a, Ord (Orbit a)) => Word a -> Word a -> Columns a -> Table a -> Bool
unequalRows s0 t0 suffs table =
  False `elem` ( productWith (\(s, t) e -> lookup (s, e) table == lookup (t, e) table) (singleOrbit (s0, t0)) suffs )


equalRows :: (Nominal a, Ord (Orbit a)) => Word a -> Word a -> Columns a -> Table a -> Bool
equalRows s0 t0 suffs table = not (unequalRows s0 t0 suffs table)

closed :: (Nominal a, Ord (Orbit a)) => Word a -> Rows a -> Columns a -> Table a -> Bool
closed t prefs suffs table =
  null (filter (\(t, s) -> unequalRows t s suffs table) (product (singleOrbit t) prefs))

nonClosedness :: (Nominal a, Ord (Orbit a)) => Rows a -> Rows a -> Columns a -> Table a -> Rows a
nonClosedness prefs prefsExt suffs table =
  filter (\t -> not (closed t prefs suffs table)) prefsExt 

inconsistencies :: (Nominal a, Ord a, Ord (Orbit a)) => Rows a -> Columns a -> Table a -> Alph a -> OrbitList (([a], [a]), (a, Word a))
inconsistencies prefs suffs table alph =
  filter (\((s, t), (a, e)) -> lookup (s ++ [a], e) table /= lookup (t ++ [a], e) table) candidatesExt
  where
    candidates = filter (\(s, t) -> s < t && equalRows s t suffs table) (product prefs prefs)
    candidatesExt = product candidates (product alph suffs)


-- Example to test
accept [Rat a, Rat b] = a == b
accept _ = False

main :: IO ()
main = do
  let alph = rationals
      prefs = singleOrbit [] `union` map (\r -> [r]) alph
      prefsExt = productWith (\p a -> p ++ [a]) prefs alph
      suffs = singleOrbit []
      table = fromSet (\(a, b) -> accept (a ++ b)) . fromOrbitList $ product (prefs `union` prefsExt) (suffs)
  print (toList . fromOrbitList $ (nonClosedness prefs prefsExt suffs table))
  print (toList . fromOrbitList $ (inconsistencies prefs suffs table alph))

