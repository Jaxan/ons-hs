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

The library uses an interface similar to the one provided by
[ONS](https://github.com/davidv1992/ONS). It provides basic instances and also
allows custom types to be nominal. It is purely functional.


## Example: Logic Solver

Since nominal sets allow us to compute with infinite sets, we can implement
a first order logic solver by trying all values in the domain.

In other words, we simply implement Tarski's truth definition:

```Haskell
data Formula = Lit Literal | T | Exists Formula | Or ...

isTrue :: Formula -> Bool
isTrue f = P.not . null $ trueFor (singleOrbit []) f  where
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

(Note that `and`, `forAll`, and `implies` can all be expressed with the above
connectives.) Here we use basic Haskell operations, however, the variable `ctx`
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
with deriving via.

For example, for the most sensible instance, use this:

```Haskell
data StateSpace = Store [Atom] | Check [Atom] | Accept | Reject
  deriving (Eq, Ord, GHC.Generic)
  deriving Nominal via Generic StateSpace
```

If, however, you want a trivial group action on your data structure. (This is
used for the data structure for equivariant sets.) Then you can use this:

```Haskell
newtype EquivariantSet a = ...
  deriving Nominal via Trivial (EquivariantSet a)
```

The type class `Nominal` provides a type family and operations on them:

```Haskell
class Nominal a where
  type Orbit a :: *
  toOrbit    :: a -> Orbit a
  getElement :: Orbit a -> Support -> a
  support    :: a -> Support
  index      :: Proxy a -> Orbit a -> Int
```

## Documentation

There is none, except this page and the comments in the code. It is on my TODO
list to write proper Haddock documentation.


## Laziness

Instead of `EquivariantSet a` it is often useful to use `OrbitList a`, since
the latter is a lazy data structure. Especially when searching for certain
values, that can be much faster.
