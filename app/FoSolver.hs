{-# LANGUAGE PatternSynonyms #-}

import OrbitList hiding (head)
import Support (Rat)

import Prelude (Show, Ord, Eq, Int, IO, print, otherwise, (.), ($), (!!), (+), (-), Bool, head, tail)
import qualified Prelude as P

{-
Solver for the FO theory of the dense linear order (i.e. Rationals with
the relation <). Implemented using nominal sets. This is in contrast to
libraries such as LOIS and nlambda, which use a solver to implement nominal
sets.
-}

-- De Bruijn indices
type Var = Int

-- Literals for the FO formulas
data Literal
  = Equals Var Var   -- There is always == in the FO language
  | LessThan Var Var -- And in our case we have 1 more relation
  deriving (Show, Ord, Eq)

-- Formulas are constructed with literals and connectives
-- Note that we only include the necessary ones (the others can be derived)
data Formula
  = Lit Literal
  | True
  | Exists Formula
  | Or Formula Formula
  | Not Formula
  deriving (Show, Ord, Eq)

pattern False = Not True

-- Smart constructors to avoid unwieldy expressions. It is a heuristic, but
-- good enough for us. The solver should be able to handle arbitrary formulas
-- anyways. We also define the other connectives in terms of the constructors.
-- I am using only the following axioms in the smart constructors:
-- * < is irreflexive
-- * The domain is non-empty
-- * De Morgan laws and classical logic
-- And nothing more, so the solver really works with the model and not the
-- theory of the dense linear order
infix 4 ==, /=, <, <=, >, >=
(==), (/=), (<), (<=), (>), (>=) :: Var -> Var -> Formula
x == y | x P.== y  = True
       | otherwise = Lit (Equals x y)
x /= y = not (x == y)
x < y  | x P.== y  = False
       | otherwise = Lit (LessThan x y)
x <= y = or (x < y) (x == y)
x > y  = y < x
x >= y = or (x > y) (x == y)

not :: Formula -> Formula
not (Not x) = x
not x = Not x

infixr 3 `and`
infixr 2 `or`
infixr 1 `implies`
and, or, implies :: Formula -> Formula -> Formula
or True x   = True
or x True   = True
or False x  = x
or x False  = x
or x y      = Or x y
and x y     = not (not x `or` not y)
implies x y = not x `or` y

-- Here I use that the domain is non-empty. Also not that these simplifications
-- don't work if we want to report a model to the user.
exists, forAll :: Formula -> Formula
exists True  = True
exists False = False
exists p     = Exists p
forAll p     = not (exists (not p))


-- Here is the solver. It keeps track of a nominal set.
-- If that sets is empty in the end, the formula does not hold.
type Context = SortedOrbitList [Rat]

extend, drop :: Context -> Context
extend ctx = productWith (:) rationals ctx
drop ctx = projectWith (\w -> (head w, tail w)) ctx

truth0 :: Context -> Formula -> Context
truth0 ctx (Lit (Equals i j))   = filter (\w -> w !! i P.== w !! j) ctx
truth0 ctx (Lit (LessThan i j)) = filter (\w -> w !! i P.<  w !! j) ctx
truth0 ctx True       = ctx
truth0 ctx (Not x)    = ctx `minus` truth0 ctx x
truth0 ctx (Or x y)   = truth0 ctx x `union` truth0 ctx y
truth0 ctx (Exists p) = drop (truth0 (extend ctx) p)

truth :: Formula -> Bool
truth f = P.not . null $ truth0 (singleOrbit []) f


-- Some tests
-- De Bruijn indices are not very easy to work with...

-- for each element there is a strictly bigger element
test1 = forAll {-1-} (exists {-0-} (1 > 0))
-- there is a minimal element (= false)
test2 = exists {-1-} (forAll {-0-} (1 <= 0))
-- the order is dense
test3 = forAll {-x-} (forAll {-y-} (1 {-x-} < 0 {-y-} `implies` exists {-z-} (2 < 0 `and` 0 < 1)))
-- Formulas by Niels
test4 = forAll (forAll (forAll (2 < 1 `and` 1 < 0 `implies`
                           (forAll (forAll (4 < 1 `and` 1 < 3 `and` 3 < 0 `and` 0 < 2) `implies`
                              (exists (2 < 0 `and` 0 < 1)))))))
test5 = exists (forAll (forAll (1 < 0 `implies`
                           exists (forAll (forAll ((1 < 0 `and` 0 < 4 `and` 3 < 1) `implies`
                                              (forAll (6 == 0))))))))
test6 = (forAll (0 == 0)) `implies` (exists (0 /= 0))
test7 = (forAll (forAll (0 == 1))) `implies` (exists (0 /= 0))

-- Used to take a long time with EquivariantSet. It's fast with lazy lists.
-- Still takes a long time if exists are replaced with forAlls.
compl :: Int -> Formula
compl n = forAll (inner n (exists (0 == (n + 1)))) where
  inner 0 f = f
  inner k f = exists (inner (k - 1) f)

main :: IO ()
main = do
  print test1
  print (truth test1)
  print test2
  print (truth test2)
  print test3
  print (truth test3)
  print test4
  print (truth test4)
  print test5
  print (truth test5)
  print test6
  print (truth test6)
  print test7
  print (truth test7)
  let n = 37
  print (compl n)
  print (truth (compl n))
