{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module OrbitList where

import qualified Data.List as L
import qualified Data.List.Ordered as LO
import Data.Proxy
import Prelude hiding (map, product)

import Nominal
import Support (Rat(..))

-- Similar to EquivariantSet, but merely a list structure. It is an
-- equivariant data type, so the Nominal instance is trivial.
newtype OrbitList a = OrbitList { unOrbitList :: [Orbit a] }
  deriving Nominal via Trivially (OrbitList a)

deriving instance Eq (Orbit a) => Eq (OrbitList a)
deriving instance Ord (Orbit a) => Ord (OrbitList a)
deriving instance Show (Orbit a) => Show (OrbitList a)

-- Simply concatenation of the list
deriving instance Semigroup (OrbitList a)
deriving instance Monoid (OrbitList a)


-- Query

null :: OrbitList a -> Bool
null (OrbitList x) = L.null x

elem :: (Nominal a, Eq (Orbit a)) => a -> OrbitList a -> Bool
elem x = L.elem (toOrbit x) . unOrbitList

-- Sizes of supports of all orbits (sorted, big to small)
size :: forall a. Nominal a => OrbitList a -> [Int]
size = LO.sortOn negate . fmap (index (Proxy :: Proxy a)) . unOrbitList

-- May fail when empty
head :: Nominal a => OrbitList a -> a
head (OrbitList l) = getElementE (L.head l)

-- Construction

empty :: OrbitList a
empty = OrbitList []

singleOrbit :: Nominal a => a -> OrbitList a
singleOrbit a = OrbitList [toOrbit a]

rationals :: OrbitList Rat
rationals = singleOrbit (Rat 0)

cons :: Nominal a => a -> OrbitList a -> OrbitList a
cons a (OrbitList l) = OrbitList (toOrbit a : l)

repeatRationals :: Int -> OrbitList [Rat]
repeatRationals 0 = singleOrbit []
repeatRationals n = productWith (:) rationals (repeatRationals (n-1))

distinctRationals :: Int -> OrbitList [Rat]
distinctRationals 0 = singleOrbit []
distinctRationals n = map (uncurry (:)) . OrbitList.separatedProduct rationals $ (distinctRationals (n-1))

increasingRationals :: Int -> OrbitList [Rat]
increasingRationals 0 = singleOrbit []
increasingRationals n = map (uncurry (:)) . OrbitList.increasingProduct rationals $ (increasingRationals (n-1))

-- Bell numbers
repeatRationalUpToPerm :: Int -> OrbitList [Rat]
repeatRationalUpToPerm 0 = singleOrbit []
repeatRationalUpToPerm 1 = map pure rationals
repeatRationalUpToPerm n = OrbitList.map (uncurry (:)) (OrbitList.increasingProduct rationals (repeatRationalUpToPerm (n-1))) <> OrbitList.map (uncurry (:)) (OrbitList.rightProduct rationals (repeatRationalUpToPerm (n-1)))

-- Map / Filter / ...

-- f should be equivariant
map :: (Nominal a, Nominal b) => (a -> b) -> OrbitList a -> OrbitList b
map f (OrbitList as) = OrbitList $ L.map (omap f) as

filter :: Nominal a => (a -> Bool) -> OrbitList a -> OrbitList a
filter f = OrbitList . L.filter (f . getElementE) . unOrbitList

partition :: Nominal a => (a -> Bool) -> OrbitList a -> (OrbitList a, OrbitList a)
partition f (OrbitList s) = both OrbitList . L.partition (f . getElementE) $ s
  where both g (a, b) = (g a, g b)

take :: Int -> OrbitList a -> OrbitList a
take n = OrbitList . L.take n . unOrbitList

-- TODO: drop, span, takeWhile, ...

-- TODO: Think about preconditions and postconditions of folds
foldr :: Nominal a => (a -> b -> b) -> b -> OrbitList a -> b
foldr f b = L.foldr (f . getElementE) b . unOrbitList

foldl :: Nominal a => (b -> a -> b) -> b -> OrbitList a -> b
foldl f b = L.foldl (\acc -> f acc . getElementE) b . unOrbitList


-- Combinations

productG :: (Nominal a, Nominal b) => (Proxy a -> Proxy b -> Orbit a -> Orbit b -> [OrbPair (OrbRec a) (OrbRec b)]) -> OrbitList a -> OrbitList b -> OrbitList (a, b)
productG f (OrbitList as) (OrbitList bs) = OrbitList . concat $ (f (Proxy :: Proxy a) (Proxy :: Proxy b) <$> as <*> bs)

product :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
product = OrbitList.productG Nominal.product

separatedProduct :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
separatedProduct = OrbitList.productG Nominal.separatedProduct

leftProduct :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
leftProduct = OrbitList.productG Nominal.leftProduct

rightProduct :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
rightProduct = OrbitList.productG Nominal.rightProduct

increasingProduct :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
increasingProduct = OrbitList.productG Nominal.increasingProduct

decreasingProduct :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
decreasingProduct = OrbitList.productG Nominal.decreasingProduct

-- Not yet the product I wish to have... That is why the name is so
-- non-informative.
testProduct :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
testProduct = OrbitList.productG Nominal.testProduct

productWith :: (Nominal a, Nominal b, Nominal c) => (a -> b -> c) -> OrbitList a -> OrbitList b -> OrbitList c
productWith f as bs = map (uncurry f) (OrbitList.product as bs)

-- TODO: productWith is the same as liftA2, so we could provide an
-- Applicative instance (with singleOrbit as pure). Although, I'm not
-- sure we can do <*>, hmm.. The Alternative instance is given by
-- concatenation (i.e. the monoid structure).

-- NOTE: only works for equivariant f! In theory, the Monad instance
-- should work over all finitely supported functions, but that's harder
-- to implement.
bind :: (Nominal a, Nominal b) => OrbitList a -> (a -> OrbitList b) -> OrbitList b
bind (OrbitList as) f = OrbitList (L.concatMap (unOrbitList . f . getElementE) as)


-- Conversions

toList :: Nominal a => OrbitList a -> [a]
toList = fmap getElementE . unOrbitList

fromList :: Nominal a => [a] -> OrbitList a
fromList = OrbitList . fmap toOrbit


-- Sorted Lists

type SortedOrbitList a = OrbitList a

-- the above map and productWith preserve ordering if `f` is order preserving
-- on orbits and filter is always order preserving

-- Combinations

union :: Ord (Orbit a) => SortedOrbitList a -> SortedOrbitList a -> SortedOrbitList a
union (OrbitList x) (OrbitList y) = OrbitList (LO.union x y)

unionAll :: Ord (Orbit a) => [SortedOrbitList a] -> SortedOrbitList a
unionAll = OrbitList . LO.unionAll . fmap unOrbitList

minus :: Ord (Orbit a) => SortedOrbitList a -> SortedOrbitList a -> SortedOrbitList a
minus (OrbitList x) (OrbitList y) = OrbitList (LO.minus x y)

-- decompose a into b and c, and then throw away b.
-- f should be equivariant and order preserving on orbits
projectWith :: (Nominal a, Nominal b, Nominal c, Eq (Orbit b), Ord (Orbit c)) => (a -> (b, c)) -> SortedOrbitList a -> SortedOrbitList c
projectWith f = unionAll . fmap OrbitList . groupOnFst . splitOrbs . unOrbitList . map f
  where
    splitOrbs = fmap (\o -> (omap fst o, omap snd o))
    groupOnFst = fmap (fmap snd) . L.groupBy (\x y -> fst x == fst y)
