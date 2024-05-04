Changelog for the `singletons-th` project
=========================================

next [????.??.??]
-----------------
* Require building with `th-desugar-1.18` or later. Notably, `th-desugar-1.18`
  now desugars all lambda, `case`, and `\case` expressions to `\cases`
  expressions, and the same principle applies to the code that `singletons-th`
  generates.

  Generally speaking, most code should continue to work after this change. Note
  that singled code might now generate `-Wunused-matches` warnings where it
  didn't before. For example, previous versions of `singletons-th` would not
  warn that the `x` in `map (\x -> ())` is unused after singling it, but this
  `singletons-th` will now generate an `-Wunused-matches` warning for the
  singled version of `x`.
* Add support for promoting and singling type variables that scope over the
  bodies of class method defaults and instance methods.

3.4 [2024.05.12]
----------------
* Require building with GHC 9.10.
* GHC 9.10 removes arity inference when kind-checking type families with
  standalone kind signatures, persuant to [this GHC
  proposal](https://github.com/ghc-proposals/ghc-proposals/blob/10290a668608d608c3f6c6010be265cf7a02e1fc/proposals/0425-decl-invis-binders.rst#breakage-2-arity-inference).
  In order to promote functions to type families with correct arities,
  `singletons-th` uses `TypeAbstractions` to bind type variable binders in the
  headers of promoted type families. As such, it is quite likely that you will
  need to enable `TypeAbstractions` in order to make GHC accept code that
  `singletons-th` generates.
* Fix a bug causing definitions with type signatures using inferred type
  variable binders (e.g., `forall a {b}. a -> b -> a`) to fail to promote.

3.3 [2023.10.13]
----------------
* Require building with GHC 9.8.
* Singled data types with derived `Eq` or `Ord` instances now generate `Eq` or
  `Ord` instances for the singleton type itself, e.g.,

  ```hs
  instance Eq (SExample a) where
    _ == _ = True

  instance Ord (SExample a) where
    compare _ _ = EQ
  ```
* `singletons-th` now makes an effort to promote definitions that use scoped
  type variables. See the "Scoped type variables" section of the `README` for
  more information about what `singletons-th` can (and can't) do.
* `singletons-th` now supports singling type-level definitions that use
  `TypeAbstractions`.
* Fix a bug in which data types using visible dependent quantification would
  generate ill-scoped code when singled.
* Fix a bug in which singling a local variable that shadows a top-level
  definition would fail to typecheck in some circumstances.
* Fix a bug in which `singletons-th` would incorrectly promote/single records
  to top-level field selectors when `NoFieldSelectors` was active.

3.2 [2023.03.12]
----------------
* Require building with GHC 9.6.
* Derived `POrd` and `SOrd` instances (arising from a use of `deriving Ord`)
  now use `(<>) @Ordering` in their implementations instead of the custom
  `thenCmp :: Ordering -> Ordering -> Ordering` function. While most code will
  likely continue to work after this change, this may break code that attempts
  to prove properties about the implementation of a derived `POrd`/`SOrd`
  instance.
* Fix a bug in which the `singDecideInstances` and `showSingInstances`, as well
  as `deriving Show` declarations, would not respect custom
  `promotedDataTypeOrConName` options.
* Allow building with `mtl-2.3.*`.

3.1.1 [2022.08.23]
------------------
* Require building with GHC 9.4.
* Improve error messages when attempting to promote a partial application of
  a function arrow `(->)`, which is not currently supported.

3.1 [2021.10.30]
----------------
* Require building with GHC 9.2.
* Allow promoting and singling type applications in data constructor patterns.
* Make the Template Haskell machinery generate `SingI1` and `SingI2` instances
  when possible.
* Make `genDefunSymbols` and related functions less likely to trigger
  [GHC#19743](https://gitlab.haskell.org/ghc/ghc/-/issues/19743).

3.0 [2021.03.12]
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
    definitions from the `base` library, including the `Prelude`. This library
    continues to require the latest stable release of GHC.

  Consult the changelogs for `singletons` and `singletons-base` for changes
  specific to those libraries. For more information on this split, see the
  [relevant GitHub discussion](https://github.com/goldfirere/singletons/issues/420).
* Require building with GHC 9.0.
* `Data.Singletons.CustomStar` and `Data.Singletons.SuppressUnusedWarnings`
  have been renamed to `Data.Singletons.TH.CustomStar` and
  `Data.Singletons.SuppressUnusedWarnings`, respectively, to give every module
  in `singletons-th` a consistent module prefix.
* Due to the `singletons` package split, the `singletons-th` modules
  `Data.Singletons.TH` and `Data.Singletons.TH.CustomStar` (formerly known as
  `Data.Singletons.CustomStar`) no longer re-export any definitions from the
  `singletons-base` module `Prelude.Singletons` (formerly known as
  `Data.Singletons.Prelude`). The `singletons-base` library now provides
  versions of these modules—`Data.Singletons.Base.CustomStar` and
  `Data.Singletons.Base.TH`, respectively—that do re-export definitions
  from `Prelude.Singletons`.
* "Fully saturated" defunctionalization symbols (e.g., `IdSym1`) are now
  defined as type families instead of type synonyms. This has two notable
  benefits:

  * Fully saturated defunctionalization symbols can now be given standalone
    kind signatures, which ensures that the order of kind variables is the
    same as the user originally declared them.
  * This fixes a minor regression in `singletons-2.7` in which the quality
    of `:kind!` output in GHCi would become worse when using promoted type
    families generated by Template Haskell.

  Under certain circumstances, this can be a breaking change:

  * Since more TH-generated promoted functions now have type families on
    their right-hand sides, some programs will now require
    `UndecidableInstances` where they didn't before.
  * Certain definitions that made use of overlapping patterns, such as
    `natMinus` below, will no longer typecheck:

    ```hs
    $(singletons [d|
      data Nat = Z | S Nat

      natMinus :: Nat -> Nat -> Nat
      natMinus Z     _     = Z
      natMinus (S a) (S b) = natMinus a b
      natMinus a     Z     = a
      |])
    ```

    This can be worked around by avoiding the use of overlapping patterns.
    In the case of `natMinus`, this amounts to changing the third equation
    to match on its first argument:

    ```hs
    $(singletons [d|
      natMinus :: Nat -> Nat -> Nat
      natMinus Z       _     = Z
      natMinus (S a)   (S b) = natMinus a b
      natMinus a@(S _) Z     = a
      |])
    ```
* The specification for how `singletons` deals with record selectors has been
  simplified. Previously, `singletons` would try to avoid promoting so-called
  "naughty" selectors (those whose types mention existential type variables
  that do not appear in the constructor's return type) to top-level functions.
  Determing if a selector is naughty is quite challenging in practice, as
  determining if a type variable is existential or not in the context of
  Template Haskell is difficult in the general case. As a result, `singletons`
  now adopts the dumb-but-predictable approach of always promoting record
  selectors to top-level functions, naughty or not.

  This means that attempting to promote code with a naughty record selector,
  like in the example below, will no longer work:

  ```hs
  $(promote [d|
    data Some :: (Type -> Type) -> Type where
      MkSome :: { getSome :: f a } -> Some f
      -- getSome is naughty due to mentioning the type variable `a`
    |])
  ```

  Please open an issue if you find this restriction burdensome in practice.
* The `singEqInstanceOnly` and `singEqInstancesOnly` functions, which generate
  `SEq` (but not `PEq`) instances, have been removed. There is not much point
  in keeping these functions around now that `PEq` now longer has a special
  default implementation. Use `singEqInstance{s}` instead.
* The Template Haskell machinery will no longer promote `TypeRep` to `Type`,
  as this special case never worked properly in the first place.
* The Template Haskell machinery will now preserve strict fields in data types
  when generating their singled counterparts.
* Introduce a new `promotedDataTypeOrConName` option to
  `Data.Singletons.TH.Options`. Overriding this option can be useful in
  situations where one wishes to promote types such as `Nat`, `Symbol`, or
  data types built on top of them. See the
  "Arrows, `Nat`, `Symbol`, and literals" section of the `README` for more
  information.
* Define a `Quote` instance for `OptionsM`. A notable benefit of this instance
  is that it avoids the need to explicitly `lift` TH quotes into `OptionsM`.
  Before, you would have to do this:

  ```hs
  import Control.Monad.Trans.Class (lift)

  withOptions defaultOptions
    $ singletons
    $ lift [d| data T = MkT |]
  ```

  But now, it suffices to simply do this:

  ```hs
  withOptions defaultOptions
    $ singletons [d| data T = MkT |]
  ```
