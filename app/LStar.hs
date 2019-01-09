{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Nominal hiding (product)
import Support (Rat(..), Support(..), intersect)
import OrbitList --(OrbitList(..), singleOrbit, product, productWith, filter, null, elem, rationals)
import qualified OrbitList as List
import EquivariantMap (EquivariantMap(..), lookup, (!))
import qualified EquivariantMap as Map
import EquivariantSet (EquivariantSet(..))
import qualified EquivariantSet as Set

import Data.List (nub)
import Control.Monad.State
import Prelude hiding (filter, null, elem, lookup, product, Word, map, take, partition)

type Word a    = [a]
type Rows a    = OrbitList (Word a)
type Columns a = OrbitList (Word a)
type Table a   = EquivariantMap (Word a) Bool

-- states, initial state, acceptance, transition
data Automaton q a = Automaton
  { states :: OrbitList q
  , initialState :: q
  , acceptance :: EquivariantMap q Bool
  , transition :: EquivariantMap (q, a) q
  }

instance (Nominal q, Nominal a, Show q, Show a) => Show (Automaton q a) where
  show Automaton{..} = 
       "{ states = " ++ show (toList states) ++
       ", initialState = " ++ show initialState ++
       ", acceptance = " ++ show (Map.toList acceptance) ++
       ", transition = " ++ show (Map.toList transition) ++
       "}"


-- Utility functions
exists f = not . null . filter f 
forAll f = null . filter (not . f)
ext p a = p <> [a]

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

quotient :: _ => EquivariantSet (a, a) -> OrbitList a -> (EquivariantMap a (Int, Support), OrbitList (Int, Support))
quotient equiv ls = go 0 Map.empty OrbitList.empty (toList ls)
  where
    go n phi acc []     = (phi, acc)
    go n phi acc (a:as) = 
      let (y0, r0) = partition (\p -> p `Set.member` equiv) (product (singleOrbit a) (fromList as))
          y1 = filter (\p -> p `Set.member` equiv)  (product (singleOrbit a) (singleOrbit a))
          y2 = map (\(a1, a2) -> (a2, (n, support a1 `intersect` support a2))) (y1 <> y0)
          m0 = Map.fromListWith (\(n1, s1) (n2, s2) -> (n1, s1 `intersect` s2)) . OrbitList.toList $ y2
          l0 = take 1 . fromList . fmap snd $ Map.toList m0
      in go (n+1) (phi <> m0) (acc <> l0) (Set.toList . Set.fromOrbitList . map snd $ r0)


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

learn :: _ => (Word a -> IO Bool) -> LStar a IO (Automaton _ _)
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
        True -> do
          let equiv  = Set.fromOrbitList . filter (\(s, t) -> equalRows s t suffs table) $ product prefs prefs
              (f, s) = quotient equiv prefs
              trans = Map.fromList . toList . map (\(s, t) -> (s, f ! t)) . filter (\(s, t) -> equalRows s t suffs table) $ product prefsExt prefs
              trans2 pa = if pa `elem` prefsExt then trans ! pa else f ! pa
          lift (print (Map.toList trans))
          return Automaton
              { states = s
              , initialState = f ! []
              , acceptance = Map.fromList . toList . map (\p -> (f ! p, table ! p)) $ prefs
              , transition = Map.fromList . toList . map (\(p, a) -> ((f ! p, a), trans2 (ext p a))) $ product prefs alph
              }


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
  aut <- evalStateT (fillTable accept >> learn accept) init
  print aut
  return ()

