`singletons`
============

[![Hackage](https://img.shields.io/hackage/v/singletons.svg)](http://hackage.haskell.org/package/singletons)

`singletons` contains the basic types and definitions needed to support
dependently typed programming techniques in Haskell. This library was
originally presented in
[_Dependently Typed Programming with Singletons_](https://richarde.dev/papers/2012/singletons/paper.pdf),
published at the Haskell Symposium, 2012.

`singletons` is intended to be a small, foundational library on which other
projects can build. As such, `singletons` has a minimal dependency
footprint and supports GHCs dating back to GHC 8.0. For more information,
consult the `singletons`
[`README`](https://github.com/goldfirere/singletons/blob/master/README.md).

You may also be interested in the following related libraries:

* The `singletons-th` library defines Template Haskell functionality that
  allows _promotion_ of term-level functions to type-level equivalents and
  _singling_ functions to dependently typed equivalents.
* The `singletons-base` library uses `singletons-th` to define promoted and
  singled functions from the `base` library, including the `Prelude`.
