{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import OnsAutomata
import OnsQuotient

import OrbitList
import qualified OrbitList as List
import EquivariantMap (EquivariantMap(..), lookup, (!))
import qualified EquivariantMap as Map
import qualified EquivariantSet as Set

import Data.List (tails)
import Control.Monad.State
import Prelude hiding (filter, null, elem, lookup, product, Word, map, take)

type Rows a    = OrbitList (Word a)
type Columns a = OrbitList (Word a)
type Table a   = EquivariantMap (Word a) Bool



-- Utility functions
exists f = not . null . filter f 
forAll f = null . filter (not . f)
ext p a = p <> [a]

equalRows :: _ => Word a -> Word a -> Columns a -> Table a -> Bool
equalRows s0 t0 suffs table =
  forAll (\((s, t), e) -> lookup (s ++ e) table == lookup (t ++ e) table) $ product (singleOrbit (s0, t0)) suffs

closed :: _ => Word a -> Rows a -> Columns a -> Table a -> Bool
closed t prefs suffs table =
  exists (\(t, s) -> equalRows t s suffs table) (product (singleOrbit t) prefs)

nonClosedness :: _ => Rows a -> Rows a -> Columns a -> Table a -> Rows a
nonClosedness prefs prefsExt suffs table =
  filter (\t -> not $ closed t prefs suffs table) prefsExt

inconsistencies :: _ => Rows a -> Columns a -> Table a -> OrbitList a -> OrbitList ((Word a, Word a), (a, Word a))
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
addRows :: _ => Rows a -> (Word a -> m Bool) -> LStar a m ()
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
addCols :: _ => Columns a -> (Word a -> m Bool) -> LStar a m ()
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

fillTable :: _ => (Word a -> m Bool) -> LStar a m ()
fillTable mq = do
  Observations{..} <- get
  let rect = product (prefs `union` prefsExt) suffs
  ans <- lift $ mapM (ask mq table) (List.toList rect)
  put $ Observations
          { table = Map.fromList ans
          , ..
          }
  return ()

learn :: _ => (Word a -> IO Bool) -> LStar a IO (Automaton _ _)
learn mq = do
  Observations{..} <- get
  let ncl = nonClosedness prefs prefsExt suffs table
      inc = inconsistencies prefs suffs table alph
  case null ncl of
    False -> do
      addRows (take 1 ncl) mq
      learn mq
    True -> do
      case null inc of
        False -> do
          addCols (take 1 (map (uncurry (:) . snd) inc)) mq
          learn mq
        True -> do
          let equiv  = Set.fromOrbitList . filter (\(s, t) -> equalRows s t suffs table) $ product prefs prefs
              (f, s) = quotient equiv prefs
              trans = Map.fromList . toList . map (\(s, t) -> (s, f ! t)) . filter (\(s, t) -> equalRows s t suffs table) $ product prefsExt prefs
              trans2 pa = if pa `elem` prefsExt then trans ! pa else f ! pa
              hypothesis = Automaton
                { states = s
                , initialState = f ! []
                , acceptance = Map.fromList . toList . map (\p -> (f ! p, table ! p)) $ prefs
                , transition = Map.fromList . toList . map (\(p, a) -> ((f ! p, a), trans2 (ext p a))) $ product prefs alph
                }
          eq <- lift (askEquiv hypothesis)
          case eq of
            Nothing -> return hypothesis
            Just w -> do
              lift (print w)
              let allSuffs = Set.fromList $ tails w
                  newSuffs = allSuffs `Set.difference` Set.fromOrbitList suffs
              addCols (Set.toOrbitList newSuffs) mq
              learn mq


accept :: _ => Word a -> IO Bool
accept w = do
  putStr "MQ \""
  putStr (toStr w)
  putStrLn "\""
  a <- getLine
  case a of
    "Y" -> return True
    "N" -> return False
    _   -> accept w

askEquiv :: _ => Automaton q a -> IO (Maybe (Word a))
askEquiv aut = do
  putStr "EQ \""
  putStr (toStr aut)
  putStrLn "\""
  a <- getLine
  case a of
    "Y"       -> return Nothing
    'N':' ':w -> return $ Just (fst $ fromStr w)
    _         -> askEquiv aut

main :: IO ()
main = do
  let alph = rationals
      prefs = singleOrbit []
      prefsExt = productWith ext prefs alph
      suffs = singleOrbit []
      table = Map.empty
      init = Observations{..}
  aut <- evalStateT (fillTable accept >> learn accept) init
  putStrLn "Done learning :D"
  return ()

