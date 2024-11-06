# ons-hs

Experimental implemtation of the ONS library in Haskell. It provides the basic
notion of nominal sets and maps, and their manipulations. It is restricted to
nominal sets which are built on rational numbers (theory: the dense linear
order, symmetry: all monotone bijections).

Nominal sets are structured possibly-infinite sets. They have symmetries which
make them finitely representable. It can be thought of as follows: given some
values from the domain (in this case: rational numbers), there are infinitely
many ways in which we can pick them. However, up to equivalence, there are
only finitely many options which are actually different. This library uses an
enumerative approach to deal with those options. In a way, the library
implements a disjunctive normal form.

Nominal sets can be used, for example, to define infinite state systems
(nominal automata). Consequently, one can then do reachability analysis,
minimisation and other automata-theoretic constructions.

This Haskell library uses an interface similar to the the C++ library
[ONS](https://github.com/davidv1992/ONS) by David Venhoek. Additionally,
`ons-hs` provides a generic way to do nominal computations on custom data
types. It is purely functional.


## Example: Logic Solver

Since nominal sets allow us to compute with infinite sets, we can implement
a first order logic solver by trying all values in the domain.

In other words, we simply implement Tarski's truth definition:

```Haskell
data Formula = Lit Literal | T | Exists Formula | Or ... | Not ... | ...

isTrue :: Formula -> Bool
isTrue f = P.not . null $ trueFor (singleOrbit []) f
  where
    -- Just check as regular Haskell values
    trueFor ctx (Lit (Equals i j))   = filter (\w -> w !! i == w !! j) ctx
    trueFor ctx (Lit (LessThan i j)) = filter (\w -> w !! i <  w !! j) ctx
    -- T is true on the whole set
    trueFor ctx T          = ctx
    -- Not is simply the complement
    trueFor ctx (Not x)    = ctx `minus` trueFor ctx x
    -- Or is the union
    trueFor ctx (Or x y)   = trueFor ctx x `union` trueFor ctx y
    -- Exists introduces a new value and then recursively checks the truth value
    trueFor ctx (Exists p) = drop (trueFor (extend ctx) p)
    extend context = productWith (:) rationals context
    drop context = map tail context
```

Note that `and`, `forAll`, and `implies` can all be expressed with the above
connectives. Here we use basic Haskell operations, however, the variable `ctx`
is of type `EquivariantSet [Atom]`. This context is an infinite set of
sequences of rational numbers. The `Exists` quantifier introduces those
rational numbers. (We are using De Bruijn indices.)

Please have a look in `app/FOSolver.hs` for more details.

Two other examples are in the `app` directory, both related to nominal automata
theory. The first is `Minimise.hs` which minimises deterministic automata.
The second is `LStar.hs` which implements the L* algorithm for nominal
automata.


## The `Nominal` type class

All of the magic is provided by the type class `Nominal`. You will rarely
need to implement this yourself, as generic instances are provided. There
are two different general instances, and you can choose which one you need
with `deriving via`.

For example, for the most sensible instance, use this:

```Haskell
data StateSpace = Store [Atom] | Check [Atom] | Accept | Reject
  deriving (Eq, Ord, Generic)
  deriving Nominal via Generically StateSpace
```

If, however, you want a trivial group action on your data structure. (This is
used for the data structure for equivariant sets.) Then you can use this:

```Haskell
newtype EquivariantSet a = ...
  deriving Nominal via Trivially (EquivariantSet a)
```

The type class `Nominal` provides a type family and operations on them:

```Haskell
class Nominal a where
  type Orbit a :: Type
  toOrbit    :: a -> Orbit a
  getElement :: Orbit a -> Support -> a
  support    :: a -> Support
  index      :: Proxy a -> Orbit a -> Int
```

## Documentation

There is none, except this README and the comments in the code. It is on my
TODO list to write proper Haddock documentation.


## Laziness

Instead of `EquivariantSet a` it is often useful to use `OrbitList a`, since
the latter is a lazy data structure. Especially when searching for certain
values, that can be much faster.


## Changelog

version 0.3.1.0 (2024-11-06):
* More types of products
* Stuff to do permutations (not only monotone ones)
* New LStar variant, which can learn equivariant (wrt permutations) languages
  with fewer queries. But it is slower.

version 0.2.3.0 (2024-11-05):
* Updates the testing and benchmarking framework.
* Replaced benchmarking dependencies, making the build process much faster.
* Added an example teacher, and run script.

version 0.2.0.0 (2024-11-01):
* Resolves compiler warnings.
* Moved from own `Generic` to `GHC.Generically` (needs base 4.17+). If you want
  to build this with an older base version, add the generically package.
* Simplifies `ons-hs.cabal` file.
* Tested with GHC 9.4.8 and 9.10.1.
* (Interestingly, GHC 9.4.8 produces faster code.)

version 0.1.0.0 (2019-02-01):
* Initial version (used in publication).
* Developed with GHC 8.X for some X.


## Copyright notice and license

Copyright 2017-2024 Joshua Moerman, Radboud Universiteit, Open Universiteit,
licensed under the EUPL (European Union Public License).

You may find the license in the `LICENSE` file. If you want to use this
library in a commercial product, or if the license is not suitable for you,
then please get in touch so that we can change the license.


## How to cite

```
@article{VenhoekMR22,
  author       = {David Venhoek and
                  Joshua Moerman and
                  Jurriaan Rot},
  title        = {Fast computations on ordered nominal sets},
  journal      = {Theor. Comput. Sci.},
  volume       = {935},
  pages        = {82--104},
  year         = {2022},
  url          = {https://doi.org/10.1016/j.tcs.2022.09.002},
  doi          = {10.1016/J.TCS.2022.09.002}
}
```
