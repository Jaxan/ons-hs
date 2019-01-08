{-# LANGUAGE DeriveGeneric #-}

module Support.Rat where

import GHC.Generics (Generic)

-- We take some model of the dense linear order. The rationals are a natural
-- choice. (Note that every countable model is order-isomorphic, so it doesn't
-- matter so much in the end.) I wrap it in a newtype, so we will only use the
-- Ord instances, and because it's not very nice to work with type synonyms.
-- Show instance included for debugging.
newtype Rat = Rat { unRat :: Rational }
  deriving (Eq, Ord, Generic)

instance Show Rat where
  show (Rat x) = show x
