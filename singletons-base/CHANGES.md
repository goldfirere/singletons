Changelog for singletons-base project
=====================================

3.0 [????.??.??]
----------------
* The `singletons` library has been split into three libraries:

  * The new `singletons` library is now a minimal library that only provides
    `Data.Singletons`, `Data.Singletons.Decide`, `Data.Singletons.Sigma`, and
    `Data.Singletons.ShowSing` (if compiled with GHC 8.6 or later).
    `singletons` now supports building GHCs back to GHC 8.0, as well as GHCJS.
  * The `singletons-th` library defines Template Haskell functionality for
    promoting and singling term-level definitions, but but nothing else. This
    library continues to require the latest stable release of GHC.
  * The `singletons-base` library defines promoted and singled versions of
    definitions from the `base` library, which includes everything under
    `Data.Singletons.Prelude.*`. This library continues to require the latest
    stable release of GHC.

  Consult the changelogs for `singletons` and `singletons-th` for changes
  specific to those libraries. For more information on this split, see the
  [relevant GitHub discussion](https://github.com/goldfirere/singletons/issues/420).
* Require building with GHC 8.12.
* Due to the `singletons` package split, the `Data.Singletons.CustomStar` and
  `Data.Singletons.TH` modules in `singletons-th` no longer re-export any
  definitions from `Data.Singletons.Prelude.*`. The `singletons-base` library
  now provides versions of these modules—`Data.Singletons.Prelude.CustomStar`
  and `Data.Singletons.Prelude.TH`, respectively—that do re-export
  definitions from `Data.Singletons.Prelude.*`.
* Due to the `singletons` package split, the `Eq`, `Ord`, etc. instances for
  `SomeSing` are no longer provided in the `Data.Singletons` module in the
  `singletons` library. Instead, they are now provided in a new
  `Data.Singletons.Prelude.SomeSing` module, which defines `Eq`, `Ord`, etc.
  instances for `SomeSing` as orphans.
* The `PEq` class no longer uses `DefaultEq` as its default implementation for
  `(==)`. `DefaultEq`, despite its name, is actually not a suitable
  implementation for `(==)` for a good majority of singleton types
  (see the discussion in
  [this GitHub issue](https://github.com/goldfirere/singletons/issues/457)
  for more information). `(==)`'s default is now defined in terms of `(/=)`,
  just like its term-level counterpart in the `Eq` class.
