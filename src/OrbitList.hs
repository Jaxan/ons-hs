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

deriving instance Eq (Orbit a) => Eq (OrbitList a)
deriving instance Ord (Orbit a) => Ord (OrbitList a)
deriving instance Show (Orbit a) => Show (OrbitList a)
deriving via (Trivial (OrbitList a)) instance Nominal (OrbitList a)

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


-- Construction

empty :: OrbitList a
empty = OrbitList []

singleOrbit :: Nominal a => a -> OrbitList a
singleOrbit a = OrbitList [toOrbit a]

rationals :: OrbitList Rat
rationals = singleOrbit (Rat 0)

repeatRationals :: Int -> OrbitList [Rat]
repeatRationals 0 = singleOrbit []
repeatRationals n = productWith (:) rationals (repeatRationals (n-1))


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
-- TODO: folds


-- Combinations

product :: forall a b. (Nominal a, Nominal b) => OrbitList a -> OrbitList b -> OrbitList (a, b)
product (OrbitList as) (OrbitList bs) = OrbitList . concat $ (Nominal.product (Proxy :: Proxy a) (Proxy :: Proxy b) <$> as <*> bs)

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
