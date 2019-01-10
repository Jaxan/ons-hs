{-# language RecordWildCards #-}
module OnsAutomata where

import Data.Char (isSpace)
import Data.Ratio
import Data.List (intersperse)

import Nominal
import Support (Rat(..), Support(..))
import OrbitList as L (OrbitList, toList)
import EquivariantMap as M (EquivariantMap, toList)

import Prelude hiding (print)


type Word a    = [a]

-- states, initial state, acceptance, transition
data Automaton q a = Automaton
  { states :: OrbitList q
  , initialState :: q
  , acceptance :: EquivariantMap q Bool
  , transition :: EquivariantMap (q, a) q
  }



-- I do not want to give weird Show instances for basic types, so I create my
-- own. This is not meant to be generic, but just enough for the queries of L*.
class ToStr a where toStr :: a -> String
class FromStr a where fromStr :: String -> (a, String)

-- Should always print integers, this is not a problem for the things we build
-- from getElementE (since it returns elements with support from 1 to n).
instance ToStr Rat where
  toStr (Rat r) = case denominator r of
    1 -> show (numerator r)
    _ -> error "Can only show integers"

instance ToStr Support where
  toStr (Support s) = "{" ++ toStr s ++ "}"

instance ToStr Bool where toStr b = show b
instance ToStr Int where toStr i = show i
instance ToStr a => ToStr [a] where
  toStr = concat . intersperse " " . fmap toStr
instance (ToStr a, ToStr b) => ToStr (a, b) where
  toStr (a, b) = "(" ++ toStr a ++ ", " ++ toStr b ++ ")" 

instance (Nominal q, Nominal a, ToStr q, ToStr a) => ToStr (Automaton q a) where
  toStr Automaton{..} =
       "{ states = " ++ toStr (L.toList states) ++
       ", initialState = " ++ toStr initialState ++
       ", acceptance = " ++ toStr (M.toList acceptance) ++
       ", transition = " ++ toStr (M.toList transition) ++ " }"

instance FromStr Rat where
  fromStr str = (Rat (read l % 1), r)
    where (l, r) = break isSpace str

instance FromStr a => FromStr [a] where
  fromStr "" = ([], "")
  fromStr str = (a : l, emptyStr)
    where
      (a, str2) = fromStr str
      (l, emptyStr)    = fromStr (dropWhile isSpace str2)

