# Bin Packing — Haskell module

I created this small Haskell module in order to learn Haskell, especially the
Data.List module.

It provides implementation of the most important algorithms for **bin packing**.

## Usage

The demo can be run directly. With GHC the `runhaskell` command may be used:

```bash
runhaskell Demo.hs
```

The BinPacking module itself can either be imported the way it is done in Demo.hs or run
interactively. In GHCi you would simply write this command:

```
:load BinPacking
```

## What is bin packing?

Bin packing is a classical problem in combinatorial optimization. You are given a set of items,
each having size between 0 and 1, and you want to pack them into the minimum possible number
of bins of unit size.

Three variants are considered in this project. You are given all the items in advance when
the offline variant is considered. This is known to be $\mathsf{NP}$-hard. In the online variant,
the items arrive one by one and you have to decide into which bin to put the item before the next one arrives.
In the $k$-bounded-space variant, you maintain a set of $k$ active bins. You can only put items into
active bins and once you deactivate a bin, you can never activate it again.

## Algorithms

* Online
  - Best Fit
  - First Fit
* Offline
  - First Fit Decreasing
  - Optimal solution with brute force
  - Optimal solution with dynamic programming (suitable for instances with a small number
    of different item sizes)
  - Asymptotic polynomial time approximation scheme (for a given ε packs the items into (1+ε)*OPT + 1 bins)
* Bounded-space version
  - Best Fit
  - Optimal solution with brute force
