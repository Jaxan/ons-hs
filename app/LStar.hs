{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Nominal hiding (product)
import Support (Rat(..))
import OrbitList --(OrbitList(..), singleOrbit, product, productWith, filter, null, elem, rationals)
import qualified OrbitList as List
import EquivariantMap (EquivariantMap(..), lookup)
import qualified EquivariantMap as Map
import qualified EquivariantSet as Set

import Control.Monad.State
import Prelude hiding (filter, null, elem, lookup, product, Word, map, take)

type Word a    = [a]
type Rows a    = OrbitList (Word a)
type Columns a = OrbitList (Word a)
type Table a   = EquivariantMap (Word a) Bool

-- Utility functions
exists f = not . null . filter f 
forAll f = null . filter (not . f)
ext = \p a -> p <> [a]

equalRows :: (Nominal a, Ord (Orbit a)) => Word a -> Word a -> Columns a -> Table a -> Bool
equalRows s0 t0 suffs table =
  forAll (\((s, t), e) -> lookup (s ++ e) table == lookup (t ++ e) table) $ product (singleOrbit (s0, t0)) suffs

closed :: (Nominal a, Ord (Orbit a)) => Word a -> Rows a -> Columns a -> Table a -> Bool
closed t prefs suffs table =
  exists (\(t, s) -> equalRows t s suffs table) (product (singleOrbit t) prefs)

nonClosedness :: (Nominal a, Ord (Orbit a)) => Rows a -> Rows a -> Columns a -> Table a -> Rows a
nonClosedness prefs prefsExt suffs table =
  filter (\t -> not $ closed t prefs suffs table) prefsExt

inconsistencies :: (Nominal a, Ord a, Ord (Orbit a)) => Rows a -> Columns a -> Table a -> OrbitList a -> OrbitList ((Word a, Word a), (a, Word a))
inconsistencies prefs suffs table alph =
  filter (\((s, t), (a, e)) -> lookup (s ++ (a:e)) table /= lookup (t ++ (a:e)) table) candidatesExt
  where
    candidates = filter (\(s, t) -> s < t && equalRows s t suffs table) (product prefs prefs)
    candidatesExt = product candidates (product alph suffs)

-- First lookup, then membership query
ask mq table (p, s) =
  let w = p ++ s in case lookup w table of
                        Just b -> return (w, b)
                        Nothing -> (w,) <$> mq w


-- invariants: * prefs and prefsExt disjoint, without dups
--             * prefsExt ordered
--             * prefs and (prefs `union` prefsExt) prefix-closed
--             * table defined on (prefs `union` prefsExt) * suffs
data Observations a = Observations
  { alph     :: OrbitList a
  , prefs    :: Rows a
  , prefsExt :: Rows a
  , suffs    :: Columns a
  , table    :: Table a
  }

-- input alphabet, inner monad, return value
type LStar i m a = StateT (Observations i) m a

-- precondition: newPrefs is subset of prefExts
addRows :: (Nominal a, Ord (Orbit a), Monad m) => Rows a -> (Word a -> m Bool) -> LStar a m ()
addRows newPrefs mq = do
  Observations{..} <- get
  let newPrefsExt = productWith ext newPrefs alph
      rect = product newPrefsExt suffs
  ans <- lift $ mapM (ask mq table) (List.toList rect)
  put $ Observations
          { prefs = prefs <> newPrefs
          , prefsExt = (prefsExt `minus` newPrefs) `union` newPrefsExt
          , table = table <> Map.fromList ans
          , ..
          }
  return ()

-- precondition: newSuffs disjoint from suffs
addCols :: (Nominal a, Ord (Orbit a), Monad m) => Columns a -> (Word a -> m Bool) -> LStar a m ()
addCols newSuffs mq = do
  Observations{..} <- get
  let rect = product (prefs `union` prefsExt) newSuffs
  ans <- lift $ mapM (ask mq table) (List.toList rect)
  put $ Observations
          { suffs = suffs <> newSuffs
          , table = table <> Map.fromList ans
          , ..
          }
  return ()

fillTable :: (Nominal a, Ord (Orbit a), Monad m) => (Word a -> m Bool) -> LStar a m ()
fillTable mq = do
  Observations{..} <- get
  let rect = product (prefs `union` prefsExt) suffs
  ans <- lift $ mapM (ask mq table) (List.toList rect)
  put $ Observations
          { table = Map.fromList ans
          , ..
          }
  return ()

learn :: _ => (Word a -> IO Bool) -> LStar a IO ()
learn mq = do
  Observations{..} <- get
  let ncl = nonClosedness prefs prefsExt suffs table
      inc = inconsistencies prefs suffs table alph
  lift (print (toList ncl))
  lift (print (toList inc))
  case null ncl of
    False -> do
      addRows (take 1 ncl) mq
      learn mq
    True -> do
      case null inc of
        False -> do
          addCols (take 1 (map (uncurry (:) . snd) inc)) mq
          learn mq
        True -> return ()


accept :: Show a => Word a -> IO Bool
accept w = do
  print w
  a <- getLine
  case a of
    "Y" -> return True
    "N" -> return False
    _   -> accept w

main :: IO ()
main = do
  let alph = rationals
      prefs = singleOrbit []
      prefsExt = productWith ext prefs alph
      suffs = singleOrbit []
      table = Map.empty
      init = Observations{..}
  evalStateT (fillTable accept >> learn accept) init
  return ()

