singletons 1.0
==============

[![Build Status](https://travis-ci.org/goldfirere/singletons.svg?branch=master)](https://travis-ci.org/goldfirere/singletons)

This is the README file for the singletons library. This file contains all the
documentation for the definitions and functions in the library.

The singletons library was written by Richard Eisenberg, eir@cis.upenn.edu, and
with significant contributions by Jan Stolarek, jan.stolarek@p.lodz.pl.  There
are two papers that describe the library. Original one, _Dependently typed
programming with singletons_, is available
[here](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf) and will
be referenced in this documentation as the "singletons paper". A follow-up
paper, _Promoting Functions to Type Families in Haskell_, is available
[here](http://www.cis.upenn.edu/~eir/papers/2014/promotion/promotion.pdf)
and will be referenced in this documentation as the
"promotion paper".

Purpose of the singletons library
---------------------------------

The library contains a definition of _singleton types_, which allow programmers
to use dependently typed techniques to enforce rich constraints among the types
in their programs. See the singletons paper for a more thorough introduction.

The package also allows _promotion_ of term-level functions to type-level
equivalents. Accordingly, it exports a Prelude of promoted and singletonized
functions, mirroring functions and datatypes found in Prelude, `Data.Bool`,
`Data.Maybe`, `Data.Either`, `Data.Tuple` and `Data.List`. See the promotion
paper for a more thorough introduction.

Compatibility
-------------

The singletons library requires GHC 7.8.2 or greater. Any code that uses the
singleton generation primitives needs to enable a long list of GHC
extensions. This list includes, but is not necessarily limited to, the
following:

* `ScopedTypeVariables`
* `TemplateHaskell`
* `TypeFamilies`
* `GADTs`
* `KindSignatures`
* `DataKinds`
* `PolyKinds`
* `TypeOperators`
* `FlexibleContexts`
* `RankNTypes`
* `UndecidableInstances`
* `FlexibleInstances`

Modules for singleton types
---------------------------

`Data.Singletons` exports all the basic singletons definitions. Import this
module if you are not using Template Haskell and wish only to define your
own singletons.

`Data.Singletons.TH` exports all the definitions needed to use the Template
Haskell code to generate new singletons.

`Data.Singletons.Prelude` re-exports `Data.Singletons` along with singleton
definitions for various Prelude types. This module provides a singletonized
equivalent of the real `Prelude`. Note that not all functions from original
`Prelude` could be turned into singletons.

`Data.Singletons.Prelude.*` modules provide singletonized equivalents of
definitions found in the following `base` library modules: `Data.Bool`,
`Data.Maybe`, `Data.Either`, `Data.List`, `Data.Tuple` and `GHC.Base`. We also
provide singletonized `Eq` and `Ord` typeclasses

`Data.Singletons.Decide` exports type classes for propositional equality.

`Data.Singletons.TypeLits` exports definitions for working with `GHC.TypeLits`.

`Data.Singletons.Void` exports a `Void` type, shamelessly copied from
Edward Kmett's `void` package, but without the great many package dependencies
in `void`.

Modules for function promotion
------------------------------

Modules in `Data.Promotion` namespace provide functionality required for
function promotion. They mostly re-export a subset of definitions from
respective `Data.Singletons` modules.

`Data.Promotion.TH` exports all the definitions needed to use the Template
Haskell code to generate promoted definitions.

`Data.Promotion.Prelude` and `Data.Promotion.Prelude.*` modules re-export all
promoted definitions from respective `Data.Singletons.Prelude`
modules. `Data.Promotion.Prelude.List` adds a significant amount of functions
that couldn't be singletonized but can be promoted. Some functions still don't
promote - these are documented in the source code of the module. There is also
`Data.Promotion.Prelude.Bounded` module that provides promoted `PBounded`
typeclass.

Functions to generate singletons
--------------------------------

The top-level functions used to generate singletons are documented in the
`Data.Singletons.TH` module. The most common case is just calling `singletons`,
which I'll describe here:

    singletons :: Q [Dec] -> Q [Dec]

Generates singletons from the definitions given. Because singleton generation
requires promotion, this also promotes all of the definitions given to the
type level.

Usage example:

```haskell
$(singletons [d|
  data Nat = Zero | Succ Nat
  pred :: Nat -> Nat
  pred Zero = Zero
  pred (Succ n) = n
  |])
```

Definitions used to support singletons
--------------------------------------

Please refer to the singletons paper for a more in-depth explanation of these
definitions. Many of the definitions were developed in tandem with Iavor Diatchki.

    data family Sing (a :: k)

The data family of singleton types. A new instance of this data family is
generated for every new singleton type.

    class SingI (a :: k) where
      sing :: Sing a

A class used to pass singleton values implicitly. The `sing` method produces
an explicit singleton value.

    data SomeSing (kproxy :: KProxy k) where
      SomeSing :: Sing (a :: k) -> SomeSing ('KProxy :: KProxy k)

The `SomeSing` type wraps up an _existentially-quantified_ singleton. Note that
the type parameter `a` does not appear in the `SomeSing` type. Thus, this type
can be used when you have a singleton, but you don't know at compile time what
it will be. `SomeSing ('KProxy :: KProxy Thing)` is isomorphic to `Thing`.

    class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
      type DemoteRep kparam :: *
      fromSing :: Sing (a :: k) -> DemoteRep kparam
      toSing   :: DemoteRep kparam -> SomeSing kparam

This class is used to convert a singleton value back to a value in the
original, unrefined ADT. The `fromSing` method converts, say, a
singleton `Nat` back to an ordinary `Nat`. The `toSing` method produces
an existentially-quantified singleton, wrapped up in a `SomeSing`.
The `DemoteRep` associated
kind-indexed type family maps a proxy of the kind `Nat`
back to the type `Nat`.

    data SingInstance (a :: k) where
      SingInstance :: SingI a => SingInstance a
    singInstance :: Sing a -> SingInstance a

Sometimes you have an explicit singleton (a `Sing`) where you need an implicit
one (a dictionary for `SingI`). The `SingInstance` type simply wraps a `SingI`
dictionary, and the `singInstance` function produces this dictionary from an
explicit singleton. The `singInstance` function runs in constant time, using
a little magic.


Equality classes
----------------

There are two different notions of equality applicable to singletons: Boolean
equality and propositional equality.

* Boolean equality is implemented in the type family `(:==)` (which is actually
a synonym for the type family `(==)` from `Data.Type.Equality`) and the class
`SEq`. See the `Data.Singletons.Prelude.Eq` module for more information.

* Propositional equality is implemented through the constraint `(~)`, the type
`(:~:)`, and the class `SDecide`. See modules `Data.Type.Equality` and
`Data.Singletons.Decide` for more information.

Which one do you need? That depends on your application. Boolean equality has
the advantage that your program can take action when two types do _not_ equal,
while propositional equality has the advantage that GHC can use the equality
of types during type inference.

Instances of both `SEq` and `SDecide` are generated when `singletons` is called
on a datatype that has `deriving Eq`. You can also generate these instances
directly through functions exported from `Data.Singletons.TH`.


Pre-defined singletons
----------------------

The singletons library defines a number of singleton types and functions
by default:

* `Bool`
* `Maybe`
* `Either`
* `Ordering`
* `()`
* tuples up to length 7
* lists

These are all available through `Data.Singletons.Prelude`. Functions that
operate on these singletons are available from modules such as `Data.Singletons.Bool`
and `Data.Singletons.Maybe`.

Promoting functions
-------------------

Function promotion allows to generate type-level equivalents of term-level
definitions. Almost all Haskell source constructs are supported -- see last
section of this README for a full list.

Promoted definitions are usually generated by calling `promote` function:

```haskell
$(promote [d|
  data Nat = Zero | Succ Nat
  pred :: Nat -> Nat
  pred Zero = Zero
  pred (Succ n) = n
  |])
```

Every promoted function and data constructor definition comes with a set of
so-called "symbols". These are required to represent partial application at the
type level. Each function gets N+1 symbols, where N is the arity. Symbols
represent application of between 0 to N arguments. When calling any of the
promoted definitions it is important refer to it using their symbol
name. Moreover, there is new function application at the type level represented
by `Apply` type family. Symbol representing arity X can have X arguments passed
in using normal function application. All other parameters must be passed by
calling `Apply`.

Users also have access to `Data.Promotion.Prelude` and its submodules (`Base`,
`Bool`, `Either`, `List`, `Maybe` and `Tuple`). These provide promoted versions
of function found in GHC's base library.

Refer to the promotion paper for more details on function promotion.

On names
--------

The singletons library has to produce new names for the new constructs it
generates. Here are some examples showing how this is done:

1. original datatype: `Nat`

   promoted kind: `Nat`

   singleton type: `SNat` (which is really a synonym for `Sing`)


2. original datatype: `:/\:`

   promoted kind: `:/\:`

   singleton type: `:%/\:`



3. original constructor: `Succ`

   promoted type: `'Succ` (you can use `Succ` when unambiguous)

   singleton constructor: `SSucc`

   symbols: `SuccSym0`, `SuccSym1`


4. original constructor: `:+:`

   promoted type: `':+:`

   singleton constructor: `:%+:`

   symbols: `:+:$`, `:+:$$`, `:+:$$$`


5. original value: `pred`

   promoted type: `Pred`

   singleton value: `sPred`

   symbols: `PredSym0`, `PredSym1`


6. original value: `+`

   promoted type: `:+`

   singleton value: `%:+`

   symbols: `:+$`, `:+$$`, `:+$$$`


Special names
-------------

There are some special cases:

1. original datatype: `[]`

   singleton type: `SList`


2.  original constructor: `[]`

    promoted type: `'[]`

    singleton constructor: `SNil`

    symbols: `NilSym0`


3. original constructor: `:`

   promoted type: `':`

   singleton constructr: `SCons`

   symbols: `ConsSym0`, `ConsSym1`


4. original datatype: `(,)`

   singleton type: `STuple2`


5. original constructor: `(,)`

   promoted type: `'(,)`

   singleton constructor: `STuple2`

   symbols: `Tuple2Sym0`, `Tuple2Sym1`, `Tuple2Sym2`

   All tuples (including the 0-tuple, unit) are treated similarly.

6. original value: `undefined`

   promoted type: `Any`

   singleton value: `undefined`


Supported Haskell constructs
----------------------------

The following constructs are fully supported:

* variables
* tuples
* constructors
* if statements
* infix expressions
* `_` patterns
* aliased patterns
* lists
* sections
* undefined
* error
* deriving Eq
* class constraints (though these sometimes fail with `let`, `lambda`, and `case`)
* literals (for `Nat` and `Symbol`)
* unboxed tuples (which are treated as normal tuples)
* records
* pattern guards
* case
* let
* lambda expressions
* `!` and `~` patterns (silently but successfully ignored during promotion)

The following constructs are supported for promotion but not singleton generation:

* class and instance declarations
* deriving of promoted `Ord` and `Bounded` instances

The following constructs are not supported:

* list comprehensions
* do
* arithmetic sequences
* datatypes that store arrows, `Nat`, or `Symbol`
* literals (limited support)

Why are these out of reach? First two depend on monads, which mention a
higher-kinded type variable. GHC does not support higher-sorted kind variables,
which would be necessary to promote/singletonize monads. There are other tricks
possible, too, but none are likely to work. See the bug report
[here](https://github.com/goldfirere/singletons/issues/37) for more info.
Arithmetic sequences are defined using `Enum` typeclass, which uses infinite
lists.

As described in the promotion paper, promotion of datatypes that store arrows is
currently impossible. So if you have a declaration such as

    data Foo = Bar (Bool -> Maybe Bool)

you will quickly run into errors.

Literals are problematic because we rely on GHC's built-in support, which
currently is limited. Functions that operate on strings will not work because
type level strings are no longer considered lists of characters. Function
working on integer literals can be promoted by rewriting them to use
`Nat`. Since `Nat` does not exist at the term level it will only be possible to
use the promoted definition, but not the original, term-level one.

This is the same line of reasoning that forbids the use of `Nat` or `Symbol`
in datatype definitions. But, see [this bug
report](https://github.com/goldfirere/singletons/issues/76) for a workaround.

Local functions whose inferred type would mention a constraint *must* have
a type signature.

Support for `*`
---------------

The built-in Haskell promotion mechanism does not yet have a full story around
the kind `*` (the kind of types that have values). Ideally, promoting some form
of `TypeRep` would yield `*`, but the implementation of TypeRep would have to be
updated for this to really work out. In the meantime, users who wish to
experiment with this feature have two options:

1) The module `Data.Singletons.TypeRepStar` has all the definitions possible for
making `*` the promoted version of `TypeRep`, as `TypeRep` is currently implemented.
The singleton associated with `TypeRep` has one constructor:

    data instance Sing (a :: *) where
      STypeRep :: Typeable a => Sing a

Thus, an implicit `TypeRep` is stored in the singleton constructor. However,
any datatypes that store `TypeRep`s will not generally work as expected; the
built-in promotion mechanism will not promote `TypeRep` to `*`.

2) The module `Data.Singletons.CustomStar` allows the programmer to define a subset
of types with which to work. See the Haddock documentation for the function
`singletonStar` for more info.

Known bugs
----------

* Fixity declarations don't promote due to GHC bug #9066. They are dropped with
  a warning
* Record updates don't singletonize

Changes from earlier versions
-----------------------------

singletons 1.0 provides promotion mechanism that supports case expressions, let
statements, anonymous functions, higher order functions and many other
features. This version of the library was published together with the promotion
paper. Version 1.0 of singletons drops GHC 7.6.3 support because romotion
requires some features that were only added in GHC 7.8 (like kind inference for
closed type families).

singletons 0.9 contains a bit of an API change from previous versions. Here is
a summary:

* There are no more "smart" constructors. Those were necessary because each
singleton used to carry both explicit and implicit versions of any children
nodes. However, this leads to exponential overhead! Now, the magic (i.e., a
use of `unsafeCoerce`) in `singInstance` gets rid of the need for storing
implicit singletons. The smart constructors did some of the work of managing
the stored implicits, so they are no longer needed.

* `SingE` and `SingRep` are gone. If you need to carry an implicit singleton,
use `SingI`. Otherwise, you probably want `SingKind`.

* The Template Haskell functions are now exported from `Data.Singletons.TH`.

* The Prelude singletons are now exported from `Data.Singletons.Prelude`.
