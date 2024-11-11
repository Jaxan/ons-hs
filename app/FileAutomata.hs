{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module FileAutomata
  ( fileAutomaton
  , formulaAutomaton
  ) where

import qualified Data.Map as M
import Data.Map ((!))
import Data.Void (Void)
import Control.Monad.Combinators.Expr
import System.Exit (exitFailure)
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import OrbitList
import Nominal
import Nominal.Class
import Nominal.Support (def)
import Automata
import qualified EquivariantSet as Set
import qualified EquivariantMap as Map

import Prelude hiding (map, product, filter)
import qualified Prelude as P


-- ***************
-- ** Utilities **
-- ***************
atoms = rationals

-- words of length == n
replicateAtoms 0 = singleOrbit []
replicateAtoms n = productWith (:) atoms (replicateAtoms (n-1))

fromKeys f = Map.fromSet f . Set.fromOrbitList

sortedAtoms n = singleOrbit (def n)


-- **************************
-- ** File data structures **
-- **************************
type Var = (Bool, Int) -- state/input + index
type Loc = Int
type Label = Int
type Dimension = Int
type MapStr = [Ordering]
type MapSup = [Bool]

data AutomatonDescr = AutomatonD
  { alphSize   :: Int
  , statesSize :: Int
  , alph       :: [(Label, Dimension)]
  , locations  :: [(Loc, Dimension, Bool)]
  , trans      :: [(Loc, Label, MapStr, Loc, MapSup)]
  } deriving Show

data Form
  = Lit Char Var Var -- '<', '=', '>'
  | And Form Form
  | Or Form Form
  | Not Form
  deriving Show

data FAutomatonDescr = FAutomaton
  { alphSize   :: Int
  , statesSize :: Int
  , alph       :: [(Label, Dimension)]
  , locations  :: [(Loc, Dimension, Bool)]
  , trans      :: [(Loc, Label, Form, Loc, [Var])]
  } deriving Show


-- ******************
-- ** File parsers **
-- ******************
type Parser = Parsec Void String

-- space consumer and lexer
sc = L.space space1 P.empty P.empty
lexeme = L.lexeme sc
symbol = L.symbol sc

-- some basic parsers
parens = between (symbol "(") (symbol ")")
arrow = symbol "->"
integer = lexeme L.decimal
boolean = lexeme binDigitChar

alphP :: Parser (Label, Dimension)
alphP = (,) <$> integer <*> integer

stateP :: Parser (Loc, Dimension, Bool)
stateP = toS <$> integer <*> integer <*> boolean where
  toS s d a = (s, d, a == '1')

transP :: Parser (Loc, Label, MapStr, Loc, MapSup)
transP = toT <$> integer <*> integer <*> lexeme (many upperChar) <* arrow <*> integer <*> lexeme (many binDigitChar) where
  toT s a ms t sup = (s, a, fmap conv ms, t, fmap ('1' ==) sup)
  conv 'A' = LT
  conv 'B' = GT
  conv 'C' = EQ

p :: Parser AutomatonDescr
p = do
  symbol "Automaton"
  alphSize <- integer
  statesSize <- integer
  symbol "Alphabet"
  alph <- count alphSize alphP
  symbol "States"
  locations <- count statesSize stateP
  symbol "Delta"
  trans <- many transP
  return AutomatonD {..}

var :: Parser Var
var = lexeme (toV <$> lowerChar <*> many digitChar) where
  toV 'x' str = (False, read str) -- state var
  toV 'y' str = (True, read str)  -- input var

transFP :: Parser (Loc, Label, Form, Loc, [Var])
transFP = toT <$> integer <*> integer <*> formP <* arrow <*> integer <*> many var where
  toT s a f t vs = (s, a, f, t, vs)

litP :: Parser Form
litP = toL <$> var <*> lexeme asciiChar <*> var where
  toL v1 c v2 = Lit c v1 v2

formP :: Parser Form
formP = makeExprParser bTerm bOperators where
  bOperators =
    [ [ Prefix (Not <$ symbol "not") ]
    , [ InfixL (And <$ symbol "and")
    ,   InfixL (Or  <$ symbol "or" ) ]
    ]
  bTerm = parens formP <|> litP

fp :: Parser FAutomatonDescr
fp = do
  symbol "FAutomaton"
  alphSize <- integer
  statesSize <- integer
  symbol "Alphabet"
  alph <- count alphSize alphP
  symbol "States"
  locations <- count statesSize stateP
  symbol "Delta"
  trans <- many transFP
  return FAutomaton {..}


-- *****************************
-- ** Conversion to Automaton **
-- *****************************
descriptionToOns :: AutomatonDescr -> (Automaton (Int, Support) (Int, Support), OrbitList (Int, Support))
descriptionToOns AutomatonD{..} = (Automaton{..}, alphabet) where
  states = mconcat [map (\w -> (l, w)) (sortedAtoms d) | (l, d, _) <- locations]
  alphabet = mconcat [map (\w -> (l, w)) (sortedAtoms d) | (l, d) <- alph]
  initialState = error "No initial state"
  sDim = M.fromList [(l, (sortedAtoms d, b)) | (l, d, b) <- locations]
  aDim = M.fromList [(l, sortedAtoms d) | (l, d) <- alph]
  dims mStr = (P.length . P.filter (/= GT) $ mStr, P.length . P.filter (/= LT) $ mStr)
  dims2 bv = P.length . P.filter id $ bv
  -- The files are exactly encoded in the way the library works
  -- But it means we have to get our hand dirty...
  transition = Map.EqMap . M.fromList $ [ (key, (val, bStr))
                      | (s, l, mStr, t, bStr) <- trans
                      , let (sd, ad) = dims mStr
                      , let k1 = OrbPair (OrbRec s) (OrbRec sd) (replicate sd GT)
                      , let k2 = OrbPair (OrbRec l) (OrbRec ad) (replicate ad GT)
                      , let key = OrbPair (OrbRec k1) (OrbRec k2) mStr
                      , let val = OrbPair (OrbRec t) (OrbRec (dims2 bStr)) (replicate (dims2 bStr) GT) ]
  acc (s, w) = let (_, b) = sDim ! s in b
  acceptance = fromKeys acc states

-- This is very similar to the NLambda code, but instead of Formula
-- we use the standard Bool type.
formToOns :: Form -> [Atom] -> [Atom] -> Bool
formToOns (Lit c (b1, n1) (b2, n2)) xs ys = op c (xys b1 !! n1) (xys b2 !! n2)
  where
    xys False = xs
    xys True = ys
    op '<' = (<)
    op '=' = (==)
    op '>' = (>)
formToOns (And f1 f2) xs ys = formToOns f1 xs ys && formToOns f2 xs ys
formToOns (Or f1 f2) xs ys = formToOns f1 xs ys || formToOns f2 xs ys
formToOns (Not f) xs ys = not (formToOns f xs ys)

varsToOns vars xs ys = [xys b !! n | (b, n) <- vars] where
  xys False = xs
  xys True = ys

fdescriptionToOns :: FAutomatonDescr -> (Automaton (Int, [Atom]) (Int, [Atom]), OrbitList (Int, [Atom]))
fdescriptionToOns FAutomaton{..} = (Automaton{..}, alphabet) where
  states = mconcat [map (\w -> (l, w)) (replicateAtoms d) | (l, d, _) <- locations]
  alphabet = mconcat [map (\w -> (l, w)) (replicateAtoms d) | (l, d) <- alph]
  initialState = error "No initial state"
  sDim = M.fromList [(l, (replicateAtoms d, b)) | (l, d, b) <- locations]
  aDim = M.fromList [(l, replicateAtoms d) | (l, d) <- alph]
  transition = Map.fromList . mconcat $ [toList . map (\(xs, ys) -> (((s, xs), (l, ys)), (t, varsToOns vars xs ys))) . filter (\(xs, ys) -> formToOns phi xs ys) $ product (fst (sDim ! s)) (aDim ! l) | (s, l, phi, t, vars) <- trans]
  acc (s, w) = let (_, b) = sDim ! s in b
  acceptance = fromKeys acc states


-- **************************
-- ** Actual file handling **
-- **************************
fileAutomaton file = do
  result <- runParser p file <$> readFile file
  case result of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      exitFailure
    Right autDescription -> do
      return $ descriptionToOns autDescription

formulaAutomaton file = do
  result <- runParser fp file <$> readFile file
  case result of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      exitFailure
    Right autDescription -> do
      return $ fdescriptionToOns autDescription

