{-# LANGUAGE ImportQualifiedPost #-}

module Permutable where

import Data.List (permutations)
import Data.Map.Strict qualified as Map

import Nominal
import Nominal.Support (toList)

---------------------------------
---------------------------------

-- Invariant: No element occurs more than once
newtype Perm = Perm (Map.Map Atom Atom)
  deriving (Eq, Ord, Show)

identity :: Perm
identity = Perm Map.empty

-- Composition (right to left)
-- TODO: check this implementation!
compose :: Perm -> Perm -> Perm
compose (Perm f) (Perm g) = reduce . Perm $ Map.compose f g <> g <> f

-- Removes elements which are mapped to itself
reduce :: Perm -> Perm
reduce (Perm f) = Perm . Map.filterWithKey (\k v -> k /= v) $ f

---------------------------------
---------------------------------

-- Invariant: The permutation only consists of elements of the support of the
-- element a.
-- This is supposed to be a monad. For now, I don't implement the Monad
-- typeclass, but do everything by hand. (I am not going to use do notation
-- anyway.)
data Permuted a = Permuted Perm a
  deriving (Eq, Ord, Show)

embed :: a -> Permuted a
embed = Permuted identity

-- to revalidate the invariant
shrink :: Nominal a => Permuted a -> Permuted a
shrink (Permuted (Perm m) a) = Permuted (Perm (Map.filter (\p -> elem p (toList (support a))) m)) a

join :: Permuted (Permuted a) -> Permuted a
join (Permuted f (Permuted g a)) = Permuted (compose f g) a

mapped :: Nominal b => (a -> b) -> Permuted a -> Permuted b
mapped fun (Permuted f a) = shrink $ Permuted f (fun a)

bind :: Nominal b => (a -> Permuted b) -> Permuted a -> Permuted b
bind comp (Permuted f a) = case comp a of
  Permuted g b -> shrink $ Permuted (compose g f) b

allPermutations :: Support -> [Perm]
allPermutations xs = fmap (reduce . Perm . Map.fromList . zip (toList xs)) . permutations $ toList xs

-- Returns a lazy list
allPermuted :: Nominal a => a -> [Permuted a]
allPermuted el = fmap (flip Permuted el) . allPermutations . support $ el

---------------------------------
---------------------------------

-- I want Nominal to be a superclass. But for now that gets in the way (as
-- Permuted is not yet a Nominal type).
-- Note that acting on an element may change its orbit (as ordered nominal
-- set).
class Permutable a where
  act :: Permuted a -> a

instance Permutable (Permuted a) where
  act = join

instance Permutable Atom where
  act (Permuted (Perm m) p) = Map.findWithDefault p p m

-- TODO: make all this generic
instance Permutable a => Permutable [a] where
  act (Permuted f ls) = fmap (\x -> act (Permuted f x)) ls

instance (Permutable a, Permutable b) => Permutable (a, b) where
  act (Permuted f (a, b)) = (act (Permuted f a), act (Permuted f b))

instance Permutable Bool where
  act (Permuted _ b) = b
