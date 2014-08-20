Bin Packing -- Haskell module
=============================

I created this small Haskell module in order to learn Haskell, especially the
Data.List module.

It provides implementation of most important algorithms for Bin Packing.

Usage
-----

The demo can be directly run; with GHC the _runhaskell_ command may be used:

    runhaskell Demo.hs

The BinPacking module itself can either be imported the way it is done in Demo.hs or run
interactively. In GHCi you would simply write this command:

    :load BinPacking


Algorithms
----------

* Online
 * Best Fit
 * First Fit
* Offline
 * First Fit Decreasing
 * Optimal solution with brute force
 * Optimal solution with dynamic programming (suitable for instances with a small number
   of different item sizes)
 * Asymptotic polynomial time approximation scheme (for a given ε packs the items into (1+ε)*OPT + 1 bins)
* Bounded-space version
 * Best Fit
 * Optimal solution with brute force
