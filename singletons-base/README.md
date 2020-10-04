`singletons-base`
=================

[![Hackage](https://img.shields.io/hackage/v/singletons-base.svg)](http://hackage.haskell.org/package/singletons-base)

`singletons-base` uses `singletons-th` to define promoted and singled
functions from the @base@ library, including the `Prelude`. This library was
originally presented in
[_Dependently Typed Programming with Singletons_](https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf),
published at the Haskell Symposium, 2012. See also
[the paper published at Haskell Symposium, 2014](https://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf),
which describes how promotion works in greater detail.

WARNING: `singletons-base` defines orphan instances for `Sing`, `SingKind`, etc.
for common types such as `Bool`, `[]`, `Maybe`, etc. If you define
instances of these types in your code, you will likely not be able to use
that code with `singletons-base`.

`singletons-base` uses code that relies on bleeding-edge GHC language
extensions. As such, `singletons-base` only supports the latest major version
of GHC (currently GHC 9.0). For more information,
consult the `singletons`
[`README`](https://github.com/goldfirere/singletons/blob/master/README.md).

You may also be interested in the following related libraries:

* The `singletons` library is a small, foundational library that defines
  basic singleton-related types and definitions.
* The `singletons-th` library defines Template Haskell functionality that
  allows _promotion_ of term-level functions to type-level equivalents and
  _singling_ functions to dependently typed equivalents.
