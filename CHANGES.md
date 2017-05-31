Changelog for singletons project
================================

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
