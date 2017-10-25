{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Orbit where

import Data.Set (Set)
import qualified Data.Set as Set

-- We only need ordering on this structure
-- I wrap it, because Rational is a type synonym
newtype Rat = Rat { unRat :: Rational }
  deriving (Eq, Ord)

-- Just for debugging
instance Show Rat where
  show (Rat r) = show r
  showsPrec n (Rat r) = showsPrec n r

-- A support is a set of rational numbers
-- Can also be represented as sorted list/vector
-- I should experiment with that, once I have some tests
type Support = Set Rat

-- Type class indicating that we can associate orbits with elements
-- In fact, it means that a is a nominal type
class Orbit a where
  data Orb a :: *
  toOrbit :: a -> Orb a
  support :: a -> Support
  -- Precondition: size of set == index
  getElement :: Orb a -> Support -> a
  -- Size of least support
  index :: Orb a -> Int

-- Just some element
getElementE :: Orbit a => Orb a -> a
getElementE orb = getElement orb (Set.fromAscList . fmap (Rat . toRational) $ [1 .. index orb])

-- Rational numbers fit the bill
instance Orbit Rat where
  data Orb Rat = OrbRational
  toOrbit _ = OrbRational
  support r = Set.singleton r
  getElement _ s
    | Set.null s = undefined
    | otherwise = Set.findMin s
  index _ = 1

deriving instance Show (Orb Rat)
deriving instance Eq (Orb Rat)
deriving instance Ord (Orb Rat)

-- Cartesian product of nominal sets as well
-- TODO: replace [Ordering] with Vec Ordering if better
instance (Orbit a, Orbit b) => Orbit (a, b) where
  data Orb (a,b) = OrbPair !(Orb a) !(Orb b) ![Ordering]
  toOrbit (a, b) = OrbPair (toOrbit a) (toOrbit b) (bla sa sb) 
    where
      sa = Set.toAscList $ support a
      sb = Set.toAscList $ support b
      bla [] ys = fmap (const GT) ys
      bla xs [] = fmap (const LT) xs
      bla (x:xs) (y:ys) = case compare x y of
        LT -> LT : (bla xs (y:ys))
        EQ -> EQ : (bla xs ys)
        GT -> GT : (bla (x:xs) ys)
  support (a, b) = Set.union (support a) (support b)
  getElement (OrbPair oa ob l) s = (getElement oa $ toSet ls, getElement ob $ toSet rs)
    where
      (ls, rs) = partitionOrd fst . zip l . Set.toAscList $ s
      toSet = Set.fromAscList . fmap snd
  index (OrbPair _ _ l) = length l

deriving instance (Show (Orb a), Show (Orb b)) => Show (Orb (a, b))
deriving instance (Eq (Orb a), Eq (Orb b)) => Eq (Orb (a, b))
deriving instance (Ord (Orb a), Ord (Orb b)) => Ord (Orb (a, b))

-- Could be in prelude or some other general purpose lib
partitionOrd :: (a -> Ordering) -> [a] -> ([a],[a])
{-# INLINE partitionOrd #-}
partitionOrd p xs = foldr (selectOrd p) ([], []) xs

selectOrd :: (a -> Ordering) -> a -> ([a], [a]) -> ([a], [a])
selectOrd f x ~(ls, rs) = case f x of
  LT -> (x : ls, rs)
  EQ -> (x : ls, x : rs)
  GT -> (ls, x : rs)

-- Enumerate all orbits in a product
-- In lexicographical order
product :: (Orbit a, Orbit b) => Orb a -> Orb b -> [Orb (a, b)]
product oa ob = OrbPair oa ob <$> prodStrings (index oa) (index ob)

prodStrings :: Int -> Int -> [[Ordering]]
prodStrings 0 0 = [[]]
prodStrings 0 n = [replicate n GT]
prodStrings n 0 = [replicate n LT]
prodStrings 1 1 = [[LT, GT], [EQ], [GT, LT]]
prodStrings n m = ((LT :) <$> prodStrings (n-1) m)
  ++ ((EQ :) <$> prodStrings (n-1) (m-1))
  ++ ((GT :) <$> prodStrings n (m-1))

-- Also for sums
instance (Orbit a, Orbit b) => Orbit (Either a b) where
  newtype Orb (Either a b) = OrbEither (Either (Orb a) (Orb b))
  toOrbit (Left a) = OrbEither (Left (toOrbit a))
  toOrbit (Right b) = OrbEither (Right (toOrbit b))
  support (Left a) = support a
  support (Right b) = support b
  getElement (OrbEither (Left oa)) s = Left (getElement oa s)
  getElement (OrbEither (Right ob)) s = Right (getElement ob s)
  index (OrbEither (Left oa)) = index oa
  index (OrbEither (Right ob)) = index ob

deriving instance (Show (Orb a), Show (Orb b)) => Show (Orb (Either a b))
deriving instance (Eq (Orb a), Eq (Orb b)) => Eq (Orb (Either a b))
deriving instance (Ord (Orb a), Ord (Orb b)) => Ord (Orb (Either a b))

-- Data structure for the discrete nominal sets
-- with a trivial action.
data Trivial a = Trivial { unTrivial :: a }

-- We need to remember the value!
instance Orbit (Trivial a) where
  newtype Orb (Trivial a) = OrbTrivial a
  toOrbit (Trivial a) = OrbTrivial a
  support _ = Set.empty
  getElement (OrbTrivial a) _ = Trivial a
  index _ = 0

deriving instance Show a => Show (Orb (Trivial a))
deriving instance Eq a => Eq (Orb (Trivial a))
deriving instance Ord a => Ord (Orb (Trivial a))

-- Orbits themselves are trivial,
-- but we need to keep track of the orbit
instance Orbit a => Orbit (Orb a) where
  newtype Orb (Orb a) = OrbOrb (Orb a)
  toOrbit a = OrbOrb a
  support _ = Set.empty
  getElement (OrbOrb oa) _ = oa
  index _ = 0

-- These are funny looking...
deriving instance Show (Orb a) => Show (Orb (Orb a))
deriving instance Eq (Orb a) => Eq (Orb (Orb a))
deriving instance Ord (Orb a) => Ord (Orb (Orb a))
