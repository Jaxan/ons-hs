{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Automata (Word)
import EquivariantMap (EquivariantMap(..), (!))
import EquivariantMap qualified as Map
import EquivariantSet qualified as Set
import ExampleAutomata
import IO
import Nominal (Nominal, Orbit, Trivially(..))
import OrbitList
import Permutable
import Quotient

import Control.Monad.State
import Data.List (tails)
import Data.Maybe (catMaybes)
import Prelude hiding (filter, null, elem, lookup, product, Word, map, take, init)
import System.IO (hFlush, stdout)

-- This is like the LStar algorithm in LStar.hs, but it skips membership
-- queries which are permutations of ones already asked. This saves a lot of
-- queries, but is slower computationally. The permutations are nicely hidden
-- away in the PermEquivariantMap data structure, so that the learning
-- algorithm is almost identical to the one in LStar.hs.
--
-- Some of the performance is regained, by using another product (now still
-- "testProduct"). I am not 100% sure this is the correct way of doing it.
-- It seems to work on the examples I tried. And I do think that something
-- along these lines potentially works.
--
-- Another way forway would be to use the `Permuted` monad, also in the
-- automaton type. But that requires more thinking.


--------------------------------------------
-- New data structure to handle permutations
newtype PermEquivariantMap k v = PEqMap { unPEqMap :: EquivariantMap k v }
  deriving Nominal via Trivially (EquivariantMap k v)

-- Defined by the join-semilattice structure of EquivariantMap, left biased.
deriving instance Ord (Orbit k) => Monoid (PermEquivariantMap k v)
deriving instance Ord (Orbit k) => Semigroup (PermEquivariantMap k v)

lookupP :: (Permutable k, Nominal k, Nominal v, _) => k -> PermEquivariantMap k v -> Maybe v
lookupP x (PEqMap m) = Map.lookup x m

-- For this algorithm, we do more lookups than inserts, so we make the insert
-- handle the permutations (at the cost of memory).
insertP :: (Nominal k, Nominal v, _) => k -> v -> PermEquivariantMap k v -> PermEquivariantMap k v
insertP k v = PEqMap . flip (Prelude.foldr (\(pk, pv) -> Map.insert pk pv)) [(pk, pv) | pkv <- allPermuted (k, v), let (pk, pv) = act pkv] . unPEqMap

(!~) :: (Permutable k, Nominal k, Nominal v, _) => PermEquivariantMap k v -> k -> v
(!~) m k = case lookupP k m of
  Just v -> v
  Nothing -> error "Key not found (in PermEquivariantMap)"


--------------------------------------------
-- From here, it's almost exactly LStar.hs

-- We use Lists, as they provide a bit more laziness
type Rows a    = OrbitList (Word a)
type Columns a = OrbitList (Word a)
type Table a   = PermEquivariantMap (Word a) Bool


-- Utility functions
exists f = not . null . filter f
forAll f = null . filter (not . f)
ext p a = p <> [a]

equalRows :: _ => Word a -> Word a -> Columns a -> Table a -> Bool
equalRows t0 s0 suffs table =
  -- I am not convinced this is right: the permutations applied here should
  -- be the same I think. As it is currently stated the permutations to s and t
  -- may be different.
  forAll (\((t, s), e) -> lookupP (s ++ e) table == lookupP (t ++ e) table) $ testProduct (singleOrbit (t0, s0)) suffs

closed :: _ => Word a -> Rows a -> Columns a -> Table a -> Bool
closed t prefs suffs table =
  exists (\(t, s) -> equalRows t s suffs table) (leftProduct (singleOrbit t) prefs)

nonClosedness :: _ => Rows a -> Rows a -> Columns a -> Table a -> Rows a
nonClosedness prefs prefsExt suffs table =
  filter (\t -> not $ closed t prefs suffs table) prefsExt

inconsistencies :: _ => Rows a -> Columns a -> Table a -> OrbitList a -> OrbitList ((Word a, Word a), (a, Word a))
inconsistencies prefs suffs table alph =
  filter (\((s, t), (a, e)) -> lookupP (s ++ (a:e)) table /= lookupP (t ++ (a:e)) table) candidatesExt
  where
    candidates = filter (\(s, t) -> s < t && equalRows s t suffs table) (testProduct prefs prefs)
    candidatesExt = testProduct candidates (product alph suffs)


-- Main state of the L* algorithm
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

-- First lookup, then membership query, also update the table
ask mq (p, s) = do
  Observations{..} <- get
  let w = p ++ s
  case lookupP w table of
    Just b -> return (w, b)
    Nothing -> do
      b <- lift (mq w)
      modify $ \o -> o { table = insertP w b table }
      return (w, b)

-- precondition: newPrefs is subset of prefExts
addRows :: _ => Rows a -> (Word a -> m Bool) -> LStar a m ()
addRows newPrefs mq = do
  Observations{..} <- get
  let newPrefsExt = productWith ext newPrefs alph
      rect = product newPrefsExt suffs
  _ <- mapM (ask mq) (OrbitList.toList rect)
  modify $ \o -> o { prefs = prefs <> newPrefs
                   , prefsExt = (prefsExt `minus` newPrefs) `union` newPrefsExt
                   }
  return ()

-- precondition: newSuffs disjoint from suffs
addCols :: _ => Columns a -> (Word a -> m Bool) -> LStar a m ()
addCols newSuffs mq = do
  Observations{..} <- get
  let rect = product (prefs `union` prefsExt) newSuffs
  _ <- mapM (ask mq) (OrbitList.toList rect)
  modify $ \o -> o { suffs = suffs <> newSuffs }
  return ()

fillTable :: _ => (Word a -> m Bool) -> LStar a m ()
fillTable mq = do
  Observations{..} <- get
  let rect = product (prefs `union` prefsExt) suffs
  _ <- mapM (ask mq) (OrbitList.toList rect)
  return ()

-- This could be cleaned up
learn :: _ => (Word a -> m Bool) -> (Automaton _ a -> m (Maybe (Word a))) -> LStar a m (Automaton _ a)
learn mq eq = do
  Observations{..} <- get
  let ncl = nonClosedness prefs prefsExt suffs table
      inc = inconsistencies prefs suffs table alph
  case null ncl of
    False -> do
      -- If not closed, then add 1 orbit of rows. Then start from top
      addRows (take 1 ncl) mq
      learn mq eq
    True -> do
      -- Closed! Now we check consistency
      case null inc of
        False -> do
          -- If not consistent, then add 1 orbit of columns. Then start from top
          addCols (take 1 (map (uncurry (:) . snd) inc)) mq
          learn mq eq
        True -> do
          -- Also consistent! Let's build a minimal automaton!
          let (f, st, _) = quotientf 0 (\s t -> s == t || equalRows s t suffs table) prefs
              trans = Map.fromList . toList . map (\(s, t) -> (s, f ! t)) . filter (\(s, t) -> equalRows s t suffs table) $ product prefsExt prefs
              trans2 pa = if pa `elem` prefsExt then trans ! pa else f ! pa
              hypothesis = Automaton
                { states = map fst st
                , initialState = f ! []
                , acceptance = Map.fromList . toList . map (\p -> (f ! p, table !~ p)) $ prefs
                , transition = Map.fromList . toList . map (\(p, a) -> ((f ! p, a), trans2 (ext p a))) $ product prefs alph
                }
              askCe = do
                ce <- lift (eq hypothesis)
                case ce of
                  Nothing -> return hypothesis
                  Just w -> do
                    let b1 = accepts hypothesis w
                    (_, b2) <- ask mq (w, [])
                    -- Ignore false counterexamples
                    case b1 == b2 of
                      True -> askCe
                      False -> do
                        -- Add all suffixes of a counterexample
                        let allSuffs = Set.fromList $ tails w
                            newSuffs = allSuffs `Set.difference` Set.fromOrbitList suffs
                        addCols (Set.toOrbitList newSuffs) mq
                        learn mq eq
          askCe


-- Here is the teacher: just pose the queries in the terminal
askMember :: _ => Word a -> IO Bool
askMember w = do
  putStrLn (toStr (MQ w))
  hFlush stdout
  a <- getLine
  case a of
    "Y" -> return True
    "N" -> return False
    _   -> askMember w

askEquiv :: _ => Automaton q a -> IO (Maybe (Word a))
askEquiv aut = do
  putStr "EQ \""
  putStr (toStr aut)
  putStrLn "\""
  hFlush stdout
  a <- getLine
  case a of
    "Y"       -> return Nothing
    'N':' ':w -> return $ Just (fst $ fromStr w)
    _         -> askEquiv aut

init alph = Observations
  { alph = alph
  , prefs = singleOrbit []
  , prefsExt = productWith ext (singleOrbit []) alph
  , suffs = singleOrbit[]
  , table = mempty
  }

main :: IO ()
main = do
  putStrLn "ALPHABET"
  hFlush stdout
  alph <- getLine
  case alph of
    "ATOMS" -> do
      aut <- evalStateT (fillTable askMember >> learn askMember askEquiv) (init rationals)
      return ()
    "FIFO" -> do
      let alph = map Put rationals `union` map Get rationals
      aut <- evalStateT (fillTable askMember >> learn askMember askEquiv) (init alph)
      return ()
    al -> do
      putStr "Unknown alphabet "
      putStrLn al
