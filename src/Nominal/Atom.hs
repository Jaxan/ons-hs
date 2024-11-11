{-# LANGUAGE DerivingVia #-}

module Nominal.Atom where

-- * Atoms

-- $Atoms
-- Module with the 'Atom' type. This is re-exported from the "Nominal" module,
-- and it often suffices to only import "Nominal".

-- | This is the type of atoms of our "ordered nominal sets" library.
-- Theoretically, you should think of atoms these as rational numbers, forming
-- a dense linear order. They can be compared for equality and order.
-- In the implementation, however, we represent them as integers, because in
-- any given situation only a finite number of atoms occur and we can choose
-- integral points. The library will always do this automatically, and I
-- noticed that in all applications, integers also suffice from the user
-- perspective.
newtype Atom = Atom {unAtom :: Int}
  deriving (Eq, Ord)
  deriving Show via Int

-- | Creates an atom with the value specified by the integer.
atom :: Int -> Atom
atom = Atom
