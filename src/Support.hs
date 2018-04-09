module Support
  ( module Support
  , module Support.OrdList
  , module Support.Rat
  ) where

import Support.OrdList
import Support.Rat

-- A support is a set of rational numbers, which can always be ordered. There
-- are several implementations: Ordered Lists, Sets, ...? This module chooses
-- the implementation. Change the import and export to experiment.

def :: Int -> Support
def n = fromDistinctAscList . fmap (Rat . toRational) $ [1..n]
