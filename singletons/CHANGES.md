Changelog for the `singletons` project
======================================

3.0.3 [2024.05.12]
------------------
* Allow building with GHC 9.10.

3.0.2 [2022.08.23]
------------------
* Allow building with GHC 9.4.
* When building with GHC 9.4 or later, use the new
  [`withDict`](https://hackage.haskell.org/package/ghc-prim-0.9.0/docs/GHC-Magic-Dict.html#v:withDict)
  primitive to implement `withSingI` instead of `unsafeCoerce`. This change
  should not have any consequences for user-facing code.

3.0.1 [2021.10.30]
------------------
* Add `SingI1` and `SingI2`, higher-order versions of `SingI`, to
  `Data.Singletons`, along with various derived functions:

  * `sing{1,2}`
  * `singByProxy{1,2}` and `singByProxy{1,2}#`
  * `usingSing{1,2}`
  * `withSing{1,2}`
  * `singThat{1,2}`

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

  Consult the changelogs for `singletons-th` and `singletons-base` for changes
  specific to those libraries. For more information on this split, see the
  [relevant GitHub discussion](https://github.com/goldfirere/singletons/issues/420).
* The internals of `ShowSing` have been tweaked to make it possible to derive
  `Show` instances for singleton types, e.g.,

  ```hs
  deriving instance ShowSing a => Show (SList (z :: [a]))
  ```

  For the most part, this is a backwards-compatible change, although there
  exists at least one corner case where the new internals of `ShowSing` require
  extra work to play nicely with GHC's constraint solver. For more details,
  refer to the Haddocks for `ShowSing'` in `Data.Singletons.ShowSing`.

2.7
---
* Require GHC 8.10.
* Record selectors are now singled as top-level functions. For instance,
  `$(singletons [d| data T = MkT { unT :: Bool } |])` will now generate this:

  ```hs
  data ST :: T -> Type where
    SMkT :: Sing b -> Sing (MkT b)

  sUnT :: Sing (t :: T) -> Sing (UnT t :: Bool)
  sUnT (SMkT sb) = sb

  ...
  ```

  Instead of this:

  ```hs
  data ST :: T -> Type where
    SMkT :: { sUnT :: Sing b } -> Sing (MkT b)
  ```

  Note that the new type of `sUnT` is more general than the previous type
  (`Sing (MkT b) -> Sing b`).

  There are two primary reasons for this change:

  1. Singling record selectors as top-level functions is consistent with how
     promoting records works (note that `MkT` is also a top-level function). As
  2. Embedding record selectors directly into a singleton data constructor can
     result in surprising behavior. This can range from simple code using a
     record selector not typechecking to the inability to define multiple
     constructors that share the same record name.

  See [this GitHub issue](https://github.com/goldfirere/singletons/issues/364)
  for an extended discussion on the motivation behind this change.
* The Template Haskell machinery now supports fine-grained configuration in
  the way of an `Options` data type, which lives in the new
  `Data.Singletons.TH.Options` module. Besides `Options`, this module also
  contains:
    * `Options`' record selectors. Currently, these include options to toggle
      generating quoted declarations, toggle generating `SingKind` instances,
      and configure how `singletons` generates the names of promoted or singled
      types. In the future, there may be additional options.
    * A `defaultOptions` value.
    * An `mtl`-like `OptionsMonad` class for monads that support carrying
      `Option`s. This includes `Q`, which uses `defaultOptions` if it is the
      top of the monad transformer stack.
    * An `OptionM` monad transformer that turns any `DsMonad` into an
      `OptionsMonad`.
    * A `withOptions` function which allows passing `Options` to TH functions
      (e.g., `promote` or `singletons`). See the `README` for a full example
      of how to use `withOptions`.
  Most TH functions are now polymorphic over `OptionsMonad` instead of
  `DsMonad`.
* `singletons` now does a much better job of preserving the order of type
  variables in type signatures during promotion and singling. See the
  `Support for TypeApplications` section of the `README` for more details.

  When generating type-level declarations in particular (e.g., promoted type
  families or defunctionalization symbols), `singletons` will likely also
  generate standalone kind signatures to preserve type variable order. As a
  result, most `singletons` code that uses Template Haskell will require the
  use of the `StandaloneKindSignatures` extension (and, by extension, the
  `NoCUSKs` extension) to work.
* `singletons` now does a more much thorough job of rejecting higher-rank types
  during promotion or singling, as `singletons` cannot support them.
  (Previously, `singletons` would sometimes accept them, often changing rank-2
  types to rank-1 types incorrectly in the process.)
* Add the `Data.Singletons.Prelude.Proxy` module.
* Remove the promoted versions of `genericTake`, `genericDrop`,
  `genericSplitAt`, `genericIndex`, and `genericReplicate` from
  `Data.Singletons.Prelude.List`. These definitions were subtly wrong since
  (1) they claim to work over any `Integral` type `i`, but in practice would
  only work on `Nat`s, and (2) wouldn't even typecheck if they were singled.
* Export `ApplyTyConAux1`, `ApplyTyConAux2`, as well as the record pattern
  synonyms selector `applySing2`, `applySing3`, etc. from `Data.Singletons`.
  These were unintentionally left out in previous releases.
* Export promoted and singled versions of the `getDown` record selector in
  `Data.Singletons.Prelude.Ord`.
* Fix a slew of bugs related to fixity declarations:
  * Fixity declarations for data types are no longer singled, as fixity
    declarations do not serve any purpose for singled data type constructors,
    which always have exactly one argument.
  * `singletons` now promotes fixity declarations for class names.
    `genPromotions`/`genSingletons` now also handle fixity declarations for
    classes, class methods, data types, and record selectors correctly.
  * `singletons` will no longer erroneously try to single fixity declarations
    for type synonym or type family names.
  * A bug that caused fixity declarations for certain defunctionalization
    symbols not to be generated has been fixed.
  * `promoteOnly` and `singletonsOnly` will now produce fixity declarations
    for values with infix names.

2.6
---
* Require GHC 8.8.
* `Sing` has switched from a data family to a type family. This
  [GitHub issue comment](https://github.com/goldfirere/singletons/issues/318#issuecomment-467067257)
  provides a detailed explanation for the motivation behind this change.

  This has a number of consequences:
  * Names like `SBool`, `SMaybe`, etc. are no longer type synonyms for
    particular instantiations of `Sing` but are instead the names of the
    singleton data types themselves. In other words, previous versions of
    `singletons` would provide this:

    ```haskell
    data instance Sing :: Bool -> Type where
      SFalse :: Sing False
      STrue  :: Sing True
    type SBool = (Sing :: Bool -> Type)
    ```

    Whereas with `Sing`-as-a-type-family, `singletons` now provides this:

    ```haskell
    data SBool :: Bool -> Type where
      SFalse :: SBool False
      STrue  :: SBool True
    type instance Sing @Bool = SBool
    ```
  * The `Sing` instance for `TYPE rep` in `Data.Singletons.TypeRepTYPE` is now
    directly defined as `type instance Sing @(TYPE rep) = TypeRep`, without the
    use of an intermediate newtype as before.
  * Due to limitations in the ways that quantified constraints and type
    families can interact
    (see [this GHC issue](https://gitlab.haskell.org/ghc/ghc/issues/14860)),
    the internals of `ShowSing` has to be tweaked in order to continue to
    work with `Sing`-as-a-type-family. One notable consequence of this is
    that `Show` instances for singleton types can no longer be derived—they
    must be written by hand in order to work around
    [this GHC bug](https://gitlab.haskell.org/ghc/ghc/issues/16365).
    This is unlikely to affect you unless you define 'Show' instances for
    singleton types by hand. For more information, refer to the Haddocks for
    `ShowSing'` in `Data.Singletons.ShowSing`.
  * GHC does not permit type class instances to mention type families, which
    means that it is no longer possible to define instances that mention the
    `Sing` type constructor. For this reason, a `WrappedSing` data type (which
    is a newtype around `Sing`) was introduced so that one can hang instances
    off of it.

    This had one noticeable effect in `singletons`
    itself: there are no longer `TestEquality Sing` or `TestCoercion Sing`
    instances. Instead, `singletons` now generates a separate
    `TestEquality`/`TestCoercion` instance for every data type that singles a
    derived `Eq` instance. In addition, the `Data.Singletons.Decide` module
    now provides top-level `decideEquality`/`decideCoercion` functions which
    provide the behavior of `testEquality`/`testCoercion`, but monomorphized
    to `Sing`. Finally, `TestEquality`/`TestCoercion` instances are provided
    for `WrappedSing`.
* GHC's behavior surrounding kind inference for local definitions has changed
  in 8.8, and certain code that `singletons` generates for local definitions
  may no longer typecheck as a result. While we have taken measures to mitigate
  the issue on `singletons`' end, there still exists code that must be patched
  on the users' end in order to continue compiling. For instance, here is an
  example of code that stopped compiling with the switch to GHC 8.8:

  ```haskell
  replicateM_ :: (Applicative m) => Nat -> m a -> m ()
  replicateM_ cnt0 f =
      loop cnt0
    where
      loop cnt
          | cnt <= 0  = pure ()
          | otherwise = f *> loop (cnt - 1)
  ```

  This produces errors to the effect of:

  ```
  • Could not deduce (SNum k1) arising from a use of ‘sFromInteger’
    from the context: SApplicative m
    ...

  • Could not deduce (SOrd k1) arising from a use of ‘%<=’
    from the context: SApplicative m
    ...
  ```

  The issue is that GHC 8.8 now kind-generalizes `sLoop` (whereas it did not
  previously), explaining why the error message mentions a mysterious kind
  variable `k1` that only appeared after kind generalization. The solution is
  to give `loop` an explicit type signature like so:

  ```diff
  -replicateM_       :: (Applicative m) => Nat -> m a -> m ()
  +replicateM_       :: forall m a. (Applicative m) => Nat -> m a -> m ()
   replicateM_ cnt0 f =
       loop cnt0
     where
  +    loop :: Nat -> m ()
       loop cnt
           | cnt <= 0  = pure ()
           | otherwise = f *> loop (cnt - 1)
  ```

  This general approach should be sufficient to fix any type inference
  regressions that were introduced between GHC 8.6 and 8.8. If this isn't the
  case, please file an issue.
* Due to [GHC Trac #16133](https://ghc.haskell.org/trac/ghc/ticket/16133) being
  fixed, `singletons`-generated code now requires explicitly enabling the
  `TypeApplications` extension. (The generated code was always using
  `TypeApplications` under the hood, but it's only now that GHC is detecting
  it.)
* `Data.Singletons` now defines a family of `SingI` instances for `TyCon1`
  through `TyCon8`:

  ```haskell
  instance (forall a.    SingI a           => SingI (f a),   ...) => SingI (TyCon1 f)
  instance (forall a b. (SingI a, SingI b) => SingI (f a b), ...) => SingI (TyCon2 f)
  ...
  ```

  As a result, `singletons` no longer generates instances for `SingI` instances
  for applications of `TyCon{N}` to particular type constructors, as they have
  been superseded by the instances above.
* Changes to `Data.Singletons.Sigma`:
  * `SSigma`, the singleton type for `Sigma`, is now defined.
  * New functions `fstSigma`, `sndSigma`, `FstSigma`, `SndSigma`, `currySigma`,
    and `uncurrySigma` have been added. A `Show` instance for `Sigma` has also
    been added.
  * `projSigma1` has been redefined to use continuation-passing style to more
    closely resemble its cousin `projSigma2`. The new type signature of
    `projSigma1` is:

    ```hs
    projSigma1 :: (forall (fst :: s). Sing fst -> r) -> Sigma s t -> r
    ```

    The old type signature of `projSigma1` can be found in the `fstSigma`
    function.
  * `Σ` has been redefined such that it is now a partial application of
    `Sigma`, like so:

    ```haskell
    type Σ = Sigma
    ```

    One benefit of this change is that one no longer needs defunctionalization
    symbols in order to partially apply `Σ`. As a result, `ΣSym0`, `ΣSym1`,
    and `ΣSym2` have been removed.
* In line with corresponding changes in `base-4.13`, the `Fail`/`sFail` methods
  of `{P,S}Monad` have been removed in favor of new `{P,S}MonadFail` classes
  introduced in the `Data.Singletons.Prelude.Monad.Fail` module. These classes
  are also re-exported from `Data.Singletons.Prelude`.
* Fix a bug where expressions with explicit signatures involving function types
  would fail to single.
* The infix names `(.)` and `(!)` are no longer mapped to `(:.)` and `(:!)`,
  as GHC 8.8 learned to parse them at the type level.
* The `Enum` instance for `SomeSing` now uses more efficient implementations of
  `enumFromTo` and `enumFromThenTo` that no longer require a `SingKind`
  constraint.

2.5.1
-----
* `ShowSing` is now a type class (with a single instance) instead of a type
  synonym. This was changed because defining `ShowSing` as a type synonym
  prevents it from working well with recursive types due to an unfortunate GHC
  bug. For more information, see
  [issue #371](https://github.com/goldfirere/singletons/issues/371).
* Add an `IsString` instance for `SomeSing`.

2.5
---
* The `Data.Promotion.Prelude.*` namespace has been removed. Use the
  corresponding modules in the `Data.Singletons.Prelude.*` namespace instead.

* Fix a regression in which certain infix type families, such as `(++)`, `($)`,
  `(+)`, and others, did not have the correct fixities.

* The default implementation of the `(==)` type in `PEq` was changed from
  `(Data.Type.Equality.==)` to a custom type family, `DefaultEq`. The reason
  for this change is that `(Data.Type.Equality.==)` is unable to conclude that
  `a == a` reduces to `True` for any `a`. (As a result, the previous version of
  `singletons` regressed in terms of type inference for the `PEq` instances
  for `Nat` and `Symbol`, which used that default.) On the other hand,
  `DefaultEq a a` _does_ reduce to `True` for all `a`.

* Add `Enum Nat`, `Show Nat`, and `Show Symbol` instances to
  `Data.Singletons.TypeLits`.

* Template Haskell-generated code may require `DataKinds` and `PolyKinds` in
  scenarios which did not previously require it:
  * `singletons` now explicitly quantifies all kind variables used in explicit
    `forall`s.
  * `singletons` now generates `a ~> b` instead of `TyFun a b -> Type` whenever
    possible.

* Since `th-desugar` now desugars all data types to GADT syntax, Template
  Haskell-generated code may require `GADTs` in situations that didn't require
  it before.

* Overhaul the way derived `Show` instances for singleton types works. Before,
  there was an awkward `ShowSing` class (which was essentially a cargo-culted
  version of `Show` specialized for `Sing`) that one had to create instances
  for separately. Now that GHC has `QuantifiedConstraints`, we can scrap this
  whole class and turn `ShowSing` into a simple type synonym:

  ```haskell
  type ShowSing k = forall z. Show (Sing (z :: k))
  ```

  Now, instead of generating a hand-written `ShowSing` and `Show` instance for
  each singleton type, we only generate a single (derived!) `Show` instance.
  As a result of this change, you will likely need to enable
  `QuantifiedConstraints` and `StandaloneDeriving` if you single any derived
  `Show` instances in your code.

* The kind of the type parameter to `SingI` is no longer specified. This only
  affects you if you were using the `sing` method with `TypeApplications`. For
  instance, if you were using `sing @Bool @True` before, then you will now need
  to now use `sing @Bool` instead.

* `singletons` now generates `SingI` instances for defunctionalization symbols
  through Template Haskell. As a result, you may need to enable
  `FlexibleInstances` in more places.

* `genDefunSymbols` is now more robust with respect to types that use
  dependent quantification, such as:

  ```haskell
  type family MyProxy k (a :: k) :: Type where
    MyProxy k (a :: k) = Proxy a
  ```

  See the documentation for `genDefunSymbols` for limitations to this.

* Rename `Data.Singletons.TypeRepStar` to `Data.Singletons.TypeRepTYPE`, and
  generalize the `Sing :: Type -> Type` instance to `Sing :: TYPE rep -> Type`,
  allowing it to work over more open kinds. Also rename `SomeTypeRepStar` to
  `SomeTypeRepTYPE`, and change its definition accordingly.

* Promoting or singling a type synonym or type family declaration now produces
  defunctionalization symbols for it. (Previously, promoting or singling a type
  synonym did nothing whatsoever, and promoting or singling a type family
  produced an error.)

* `singletons` now produces fixity declarations for defunctionalization
  symbols when appropriate.

* Add `(%<=?)`, a singled version of `(<=?)` from `GHC.TypeNats`, as well as
  defunctionalization symbols for `(<=?)`, to `Data.Singletons.TypeLits`.

* Add `Data.Singletons.Prelude.{Semigroup,Monoid}`, which define
  promoted and singled versions of the `Semigroup` and `Monoid` type classes,
  as well as various newtype modifiers.

  `Symbol` is now has promoted `Semigroup` and `Monoid` instances as well.
  As a consequence, `Data.Singletons.TypeLits` no longer exports `(<>)` or
  `(%<>)`, as they are superseded by the corresponding methods from
  `PSemigroup` and `SSemigroup`.

* Add promoted and singled versions of the `Functor`, `Foldable`,
  `Traversable`, `Applicative`, `Alternative`, `Monad`, `MonadPlus`, and
  `MonadZip` classes. Among other things, this grants the ability to promote
  or single `do`-notation and list comprehensions.
  * `Data.Singletons.Prelude.List` now reexports more general
    `Foldable`/`Traversable` functions wherever possible, just as `Data.List`
    does.

* Add `Data.Singletons.Prelude.{Const,Identity}`, which define
  promoted and singled version of the `Const` and `Identity` data types,
  respectively.

* Promote and single the `Down` newtype in `Data.Singletons.Prelude.Ord`.

* To match the `base` library, the promoted/singled versions of `comparing`
  and `thenCmp` are no longer exported from `Data.Singletons.Prelude`. (They
  continue to live in `Data.Singletons.Prelude.Ord`.)

* Permit singling of expression and pattern signatures.

* Permit promotion and singling of `InstanceSigs`.

* `sError` and `sUndefined` now have `HasCallStack` constraints, like their
  counterparts `error` and `undefined`. The promoted and singled counterparts
  to `errorWithoutStackTrace` have also been added in case you do not want
  this behavior.

* Add `Data.Singletons.TypeError`, which provides a drop-in replacement for
  `GHC.TypeLits.TypeError` which can be used at both the value- and type-level.

2.4.1
-----
* Restore the `TyCon1`, `TyCon2`, etc. types. It turns out that the new
`TyCon` doesn't work with kind-polymorphic tycons.

2.4
---
* Require GHC 8.4.

* `Demote Nat` is now `Natural` (from `Numeric.Natural`) instead of `Integer`.
  In accordance with this change, `Data.Singletons.TypeLits` now exposes
  `GHC.TypeNats.natVal` (which returns a `Natural`) instead of
  `GHC.TypeLits.natVal` (which returns an `Integer`).

* The naming conventions for infix identifiers (e.g., `(&*)`) have been overhauled.
  * Infix functions (that are not constructors) are no longer prepended with a
    colon when promoted to type families. For instance, the promoted version of
    `(&*)` is now called `(&*)` as well, instead of `(:&*)` as before.

    There is one exception to this rule: the `(.)` function, which is promoted
    as `(:.)`. The reason is that one cannot write `(.)` at the type level.
  * Singletons for infix functions are now always prepended with `%` instead of `%:`.
  * Singletons for infix classes are now always prepended with `%` instead of `:%`.
  * Singletons for infix datatypes are now always prepended with a `%`.

    (Before, there was an unspoken requirement that singling an infix datatype
    required that name to begin with a colon, and the singleton type would begin
    with `:%`. But now that infix datatype names can be things like `(+)`, this
    requirement became obsolete.)

  The upshot is that most infix names can now be promoted using the same name, and
  singled by simply prepending the name with `%`.

* The suffix for defunctionalized names of symbolic functions (e.g., `(+)`) has
  changed. Before, the promoted type name would be suffixed with some number of
  dollar signs (e.g., `(+$)` and `(+$$)`) to indicate defunctionalization
  symbols. Now, the promoted type name is first suffixed with `@#@` and
  _then_ followed by dollar signs (e.g., `(+@#@$)` and `(+@#@$$)`).
  Adopting this conventional eliminates naming conflicts that could arise for
  functions that consisted of solely `$` symbols.

* The treatment of `undefined` is less magical. Before, all uses of `undefined`
  would be promoted to `GHC.Exts.Any` and singled to `undefined`. Now, there is
  a proper `Undefined` type family and `sUndefined` singleton function.

* As a consequence of not promoting `undefined` to `Any`, there is no need to
  have a special `any_` function to distinguish the function on lists. The
  corresponding promoted type, singleton function, and defunctionalization
  symbols are now named `Any`, `sAny`, and `AnySym{0,1,2}`.

* Rework the treatment of empty data types:
  * Generated `SingKind` instances for empty data types now use `EmptyCase`
    instead of simply `error`ing.
  * Derived `PEq` instances for empty data types now return `True` instead of
    `False`. Derived `SEq` instances now return `True` instead of `error`ing.
  * Derived `SDecide` instances for empty data types now return `Proved bottom`,
    where `bottom` is a divergent computation, instead of `error`ing.

* Add `Data.Singletons.Prelude.IsString` and `Data.Promotion.Prelude.IsString`
  modules. `IsString.fromString` is now used when promoting or singling
  string literals when the `-XOverloadedStrings` extension is enabled
  (similarly to how `Num.fromInteger` is currently used when promoting or
  singling numeric literals).

* Add `Data.Singletons.Prelude.Void`.

* Add promoted and singled versions of `div`, `mod`, `divMod`, `quot`, `rem`,
  and `quotRem` to `Data.Singletons.TypeLits` that utilize the efficient `Div`
  and `Mod` type families from `GHC.TypeNats`. Also add `sLog2` and
  defunctionalization symbols for `Log2` from `GHC.TypeNats`.

* Add `(<>)` and `(%<>)`, the promoted and singled versions of `AppendSymbol`
  from `GHC.TypeLits`.

* Add `(%^)`, the singleton version of `GHC.TypeLits.^`.

* Add `unlines` and `unwords` to `Data.Singletons.Prelude.List`.

* Add promoted and singled versions of `Show`, including `deriving` support.

* Add a `ShowSing` class, which facilitates the ability to write `Show` instances
  for `Sing` instances.

* Permit derived `Ord` instances for empty datatypes.

* Permit standalone `deriving` declarations.

* Permit `DeriveAnyClass` (through the `anyclass` keyword of `DerivingStrategies`)

* Add a value-level `(@@)`, which is a synonym for `applySing`.

* Add `Eq`, `Ord`, `Num`, `Enum`, and `Bounded` instances for `SomeSing`, which
  leverage the `SEq`, `SOrd`, `SNum`, `SEnum`, and `SBounded` instances,
  respectively, for the underlying `Sing`.

* Rework the `Sing (a :: *)` instance in `Data.Singletons.TypeRepStar` such
  that it now uses type-indexed `Typeable`. The new `Sing` instance is now:

  ```haskell
  newtype instance Sing :: Type -> Type where
    STypeRep :: TypeRep a -> Sing a
  ```

  Accordingly, the `SingKind` instance has also been changed:

  ```haskell
  instance SingKind Type where
    type Demote Type = SomeTypeRepStar
    ...

  data SomeTypeRepStar where
    SomeTypeRepStar :: forall (a :: *). !(TypeRep a) -> SomeTypeRepStar
  ```

  Aside from cleaning up some implementation details, this change assures
  that `toSing` can only be called on `TypeRep`s whose kind is of kind `*`.
  The previous implementation did not enforce this, which could lead to
  segfaults if used carelessly.

* Instead of `error`ing, the `toSing` implementation in the `SingKind (k1 ~> k2)`
  instance now works as one would expect (provided the user adheres to some
  common-sense `SingKind` laws, which are now documented).

* Add a `demote` function, which is a convenient shorthand for `fromSing sing`.

* Add a `Data.Singletons.Sigma` module with a `Sigma` (dependent pair) data type.

* Export defunctionalization symbols for `Demote`, `SameKind, `KindOf`, `(~>)`,
  `Apply`, and `(@@)` from `Data.Singletons`.

* Add an explicitly bidirectional pattern synonym `Sing`. Pattern
  matching on `Sing` brings a `SingI ty` constraint into scope from a
  singleton `Sing ty`.

* Add an explicitly bidirectional pattern synonym `FromSing`. Pattern
  matching on any demoted (base) type gives us the corresponding
  singleton.

* Add explicitly bidirectional pattern synonyms
  `SLambda{2..8}`. Pattern matching on any defunctionalized singleton
  yields a term-level Haskell function on singletons.

* Remove the family of `TyCon1`, `TyCon2`, ..., in favor of just `TyCon`.
  GHC 8.4's type system is powerful enough to allow this nice simplification.

2.3
---
* Documentation clarifiation in `Data.Singletons.TypeLits`, thanks to @ivan-m.

* `Demote` was no longer a convenient way of calling `DemoteRep` and has been
removed. `DemoteRep` has been renamed `Demote`.

* `DemoteRep` is now injective.

* Demoting a `Symbol` now gives `Text`. This is motivated by making `DemoteRep`
  injective. (If `Symbol` demoted to `String`, then there would be a conflict
  between demoting `[Char]` and `Symbol`.)

* Generating singletons also now generates fixity declarations for the singletonized
  definitions, thanks to @int-index.

* Though more an implementation detail: singletons no longer uses kind-level proxies anywhere,
  thanks again to @int-index.

* Support for promoting higher-kinded type variables, thanks for @int-index.

* `Data.Singletons.TypeLits` now exports defunctionalization symbols for `KnownNat`
and `KnownSymbol`.

* Better type inference support around constraints, as tracked in Issue #176.

* Type synonym definitions are now ignored, as they should be.

* `Show` instances for `SNat` and `SSymbol`, thanks to @cumber.

* The `singFun` and `unSingFun` functions no longer use proxies, preferring
  `TypeApplications`.

2.2
---
* With `TypeInType`, we no longer kind `KProxy`. @int-index has very helpfully
removed the use of `KProxy` from `singletons`.

* Drop support for GHC 7.x.

* Remove `bugInGHC`. That function was intended to work around GHC's difficulty
in detecting exhaustiveness of GADT pattern matches. GHC 8 comes with a much
better exhaustiveness checker, and so this function is no longer necessary.

2.1
---
* Require `th-desugar` >= 1.6

* Work with GHC 8. GHC 8 gives the opportunity to simplify some pieces of
singletons, but these opportunities are not yet fully realized. For example,
injective type families means that we no longer need `Sing` to be a data
family; it could be a type family. This might drastically simplify the way
functions are singletonized. But not yet!

* `singletons` now outputs a few more type/kind annotations to help GHC do
type inference. There may be a few more programs accepted than before.
(This is the fix for #136.)

2.0.1
-----
 * Lots more functions in `Data.Singletons.Prelude.List`:
   `filter`, `find`, `elemIndex`, `elemIndices`, `findIndex`, `findIndices`,
   `intersect`, `intersectBy`, `takeWhile`, `dropWhile`, `dropWhileEnd`,
   `span`, `break`, `take`, `drop`, `splitAt`, `group`, `maximum`,
   `minimum`, `insert`, `sort`, `groupBy`, `lookup`, `partition`,
   `sum`, `product`, `length`, `replicate`, `transpose`, `(!!)`,
   `nub`, `nubBy`, `unionBy`, `union`, `genericLength`

2.0.0.2
-------
 * Fix fixity of `*`.

2.0.0.1
-------
 * Make haddock work.

2.0
---

* Instance promotion now works properly -- it was quite buggy in 1.0.

* Classes and instances can now be singletonized.

* Limited support for functional dependencies.

* We now have promoted and singletonized versions of `Enum`, as well as `Bounded`.

* Deriving `Enum` is also now supported.

* Ditto for `Num`, which includes an instance for `Nat`, naturally.

* Promoting a literal number now uses overloaded literals at the type level,
using a type-level `FromInteger` in the type-level `Num` class.

* Better support for dealing with constraints. Some previously-unsingletonizable
functions that have constrained parameters now work.

* No more orphan `Quasi` instances!

* Support for functions of arity 8 (instead of the old limit, 7).

* Full support for fixity declarations.

* A raft of bugfixes.

* Drop support for GHC 7.8. You must have GHC 7.10.2.

1.1.2.1
-------

Fix bug #116, thus allowing locally-declared symbols to be used in GHC 7.10.

1.1.2
-----

* No more GHC 7.8.2 support -- you must have GHC 7.8.3.

1.1.1
-----

Update testsuite to work with th-desugar-1.5.2. No functional changes.

1.1
---

This is a maintenance release to support building (but *not* testing, due to
GHC bug #10058) with 7.10. This release also targets th-desugar-1.5. Some
types changed (using th-desugar's new `DsMonad` instead of `Quasi`), but
clients generally won't need to make any changes, unless they, too, generalize
over `Quasi`.

1.0
---

This is a complete rewrite of the package.

* A much wider array of surface syntax is now accepted for promotion
and singletonization, including `let`, `case`, partially-applied functions,
and anonymous functions, `where`, sections, among others.

* Classes and instances can be promoted (but not singletonized).

* Derivation of promoted instances for `Ord` and `Bounded`.

This release can be seen as a "technology preview". More features are coming
soon.

This version drops GHC 7.6 support.

0.10.0
------

Template Haskell names are now more hygienic. In other words, `singletons`
won't try to gobble up something happened to be named `Sing` in your project.
(Note that the Template Haskell names are not *completely* hygienic; names
generated during singleton generation can still cause conflicts.)

If a function to be promoted or singletonized is missing a type signature,
that is now an *error*, not a warning.

Added a new external module Data.Singletons.TypeLits, which contain the
singletons for GHC.TypeLits. Some convenience functions are also provided.

The extension `EmptyCase` is no longer needed. This caused pain when trying
to support both GHC 7.6.3 and 7.8.

0.9.3
-----

Fix export list of Data.Singletons.TH, again again.

Add `SEq` instances for `Nat` and `Symbol`.

0.9.2
-----

Fix export list of Data.Singletons.TH, again.

0.9.1
-----

Fix export list of Data.Singletons.TH.

0.9.0
-----

Make compatible with GHC HEAD, but HEAD reports core lint errors sometimes.

Change module structure significantly. If you want to derive your own
singletons, you should import `Data.Singletons.TH`. The module
`Data.Singletons` now exports functions only for the *use* of singletons.

New modules `Data.Singletons.Bool`, `...Maybe`, `...Either`, and `...List`
are just like their equivalents from `Data.`, except for `List`, which is
quite lacking in features.

For singleton equality, use `Data.Singletons.Eq`.

For propositional singleton equality, use `Data.Singletons.Decide`.

New module `Data.Singletons.Prelude` is meant to mirror the Haskell Prelude,
but with singleton definitions.

Streamline representation of singletons, resulting in *exponential* speedup
at execution. (This has not been rigorously measured, but the data structures
are now *exponentially* smaller.)

Add internal support for TypeLits, because the TypeLits module no longer
exports singleton definitions.

Add support for existential singletons, through the `toSing` method of
`SingKind`.

Remove the `SingE` class, bundling its functionality into `SingKind`.
Thus, the `SingRep` synonym has also been removed.

Name change: `KindIs` becomes `KProxy`.

Add support for singletonizing calls to `error`.

Add support for singletonizing empty data definitions.

0.8.6
-----

Make compatible with GHC HEAD, but HEAD reports core lint errors sometimes.

0.8.5
-----

Bug fix to make singletons compatible with GHC 7.6.1.

Added git info to cabal file.

0.8.4
-----

Update to work with latest version of GHC (7.7.20130114).

Now use branched type family instances to allow for promotion of functions
with overlapping patterns.

Permit promotion of functions with constraints by omitting constraints.

0.8.3
-----

Update to work with latest version of GHC (7.7.20121031).

Removed use of Any to simulate kind classes; now using KindOf and OfKind
from GHC.TypeLits.

Made compatible with GHC.TypeLits.

0.8.2
-----

Added this changelog

Update to work with latest version of GHC (7.6.1). (There was a change to
Template Haskell).

Moved library into Data.Singletons.

0.8.1
-----

Update to work with latest version of GHC. (There was a change to
Template Haskell).

Updated dependencies in cabal to include the newer version of TH.

0.8
---

Initial public release
