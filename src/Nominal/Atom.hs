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

{- Notes:

- This type originally started out as Haskell's 'Rational' type. But since
  all representatives would be computed by the library (e.g., in
  'EquivariantSet'), the chosen atoms were always integers. Even in the
  applications, such as automata learning, it is always possible to chose
  integers. For that reason, the type is changed to 'Int'.

- The change from 'Rational' (which is 'Ration Integer', so a big number
  type) to 'Int' did increase performance a little bit, around 5%. I guess
  it also reduces the memory footprint, but I have not measured this.

- Most of the computations work on 'Orbit's anyways, and do not require
  any representatives.

-}
