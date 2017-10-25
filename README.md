# ons-hs

Experimental implemtation of the ONS library in Haskell. It provides the basic
notion of nominal sets and maps, and their manipulations (over the total order
symmetry). It is implemented by instantiating the known representation theory,
which turns out to be quite easy for the total order symmetry.

Nominal sets are structured possibly-infinite sets. They have symmetries which
make them finitely representable. They can be used, for example, to define
infinite state systems (nominal automata). Consequently, one can then do
reachability analysis, minimisation and other automata-theoretic constructions.
Here we take nominal sets over the total order symmetry, which means that the
data-values come from the rational numbers (or any other dense linear order).
The symmetries which act upon the nominal sets are the monotone bijections on
the rationals.

The library uses an interface similar to the one provided by
[ONS](https://github.com/davidv1992/ONS). It provides basic instances and also
allows custom types to be nominal. It is purely functional.


## TODO

This will never be a fully fledged library. It is just for fun. Nevertheless, I
wish to do the following:

* Provide generic instances
* Figure out the right data-structures (List vs. Sets vs. Seqs vs. ...)
* Cleanup
* Examples
* Tests
