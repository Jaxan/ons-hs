{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module OrbitList where

import qualified Data.List as L
import qualified Data.List.Ordered as LO
import Data.Proxy
import Prelude hiding (map, product)

import Nominal
import Support


newtype OrbitList a = OrbitList { unOrbitList :: [Orbit a] }

deriving instance Eq (Orbit a) => Eq (OrbitList a)
deriving instance Ord (Orbit a) => Ord (OrbitList a)
deriving instance Show (Orbit a) => Show (OrbitList a)

null :: OrbitList a -> Bool
null (OrbitList x) = L.null x

empty :: OrbitList a
empty = OrbitList []

singleOrbit :: Nominal a => a -> OrbitList a
singleOrbit a = OrbitList [toOrbit a]

rationals :: OrbitList Rat
rationals = singleOrbit (Rat 0)

-- f should be equivariant
map :: (Nominal a, Nominal b) => (a -> b) -> OrbitList a -> OrbitList b
map f (OrbitList as) = OrbitList $ L.map (omap f) as

productWith :: forall a b c. (Nominal a, Nominal b, Nominal c) => (a -> b -> c) -> OrbitList a -> OrbitList b -> OrbitList c
productWith f (OrbitList as) (OrbitList bs) = map (uncurry f) (OrbitList (concat $ product (Proxy :: Proxy a) (Proxy :: Proxy b) <$> as <*> bs))

filter :: Nominal a => (a -> Bool) -> OrbitList a -> OrbitList a
filter f = OrbitList . L.filter (f . getElementE) . unOrbitList


type SortedOrbitList a = OrbitList a
-- the above map and productWith preserve ordering if `f` is order preserving
-- on orbits and filter is always order preserving

union :: Ord (Orbit a) => SortedOrbitList a -> SortedOrbitList a -> SortedOrbitList a
union (OrbitList x) (OrbitList y) = OrbitList (LO.union x y)

unionAll :: Ord (Orbit a) => [SortedOrbitList a] -> SortedOrbitList a
unionAll = OrbitList . LO.unionAll . fmap unOrbitList

minus :: Ord (Orbit a) => SortedOrbitList a -> SortedOrbitList a -> SortedOrbitList a
minus (OrbitList x) (OrbitList y) = OrbitList (LO.minus x y)

-- decompose a into b and c (should be order preserving), and then throw away b
projectWith :: (Nominal a, Nominal b, Nominal c, Eq (Orbit b), Ord (Orbit c)) => (a -> (b, c)) -> SortedOrbitList a -> SortedOrbitList c
projectWith f = unionAll . fmap OrbitList . groupOnFst . splitOrbs . unOrbitList . map f
  where
    splitOrbs = fmap (\o -> (omap fst o, omap snd o))
    groupOnFst = fmap (fmap snd) . L.groupBy (\x y -> fst x == fst y)
