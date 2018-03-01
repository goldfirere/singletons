Changelog for singletons project
================================

2.5
---
* Fix a regression in which certain infix type families, such as `(++)`, `($)`,
  `(+)`, and others, did not have the correct fixities.

* Fix a regression in which `a == a` no longer held when `a` was a `Nat` or a
  `Symbol`.

* Template Haskell-generated code may require `TypeInType` in scenarios which
  did not previously require it:
  * `singletons` now explicitly quantifies all kind variables used in explicit
    `forall`s.
  * `singletons` now generates `a ~> b` instead of `TyFun a b -> Type` whenever
    possible.

* Since `th-desugar` now desugars all data types to GADT syntax, Template
  Haskell-generated code may require `GADTs` in situations that didn't require
  it before.

* `singletons` now generates `SingI` instances for defunctionalization symbols
  through Template Haskell. As a result, you may need to enable
  `FlexibleInstances` in more places.

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

* Add `Data.{Promotion,Singletons}.Prelude.{Semigroup,Monoid}`, which define
  promoted and singled versions of the `Semigroup` and `Monoid` type classes,
  as well as various newtype modifiers.

  `Symbol` is now has promoted `Semigroup` and `Monoid` instances as well.
  As a consequence, `Data.Singletons.TypeLits` no longer exports `(<>)` or
  `(%<>)`, as they are superseded by the corresponding methods from
  `PSemigroup` and `SSemigroup`.

* Promote and single the `Down` newtype in `Data.Singletons.Prelude.Ord`.

* To match the `base` library, the promoted/singled versions of `comparing`
  and `thenCmp` are no longer exported from `Data.Singletons.Prelude`. (They
  continue to live in `Data.Singletons.Prelude.Ord`.)

* Permit singling of expression and pattern signatures.

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
