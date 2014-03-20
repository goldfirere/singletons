singletons 0.10
===============

[![Build Status](https://travis-ci.org/goldfirere/singletons.svg?branch=master)](https://travis-ci.org/goldfirere/singletons)

This is the README file for the singletons library. This file contains all the
documentation for the definitions and functions in the library.

The singletons library was written by Richard Eisenberg, eir@cis.upenn.edu.
See also _Dependently typed programming with singletons_, available
[here](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf).

Purpose of the singletons library
---------------------------------

The library contains a definition of _singleton types_, which allow
programmers to use dependently typed techniques to enforce rich constraints
among the types in their programs. See the paper cited above for a
more thorough introduction.

Compatibility
-------------

The singletons library requires GHC version 7.6.3 or greater.
Any code that uses the singleton generation primitives will also need
to enable a long list of GHC extensions. This list includes, but
is not necessarily limited to, the following:

* `ScopedTypeVariables` (absolutely required)
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

Modules
-------

`Data.Singletons` exports all the basic singletons definitions. Import this
module if you are not using Template Haskell and wish only to define your
own singletons.

`Data.Singletons.TH` exports all the definitions needed to use the Template
Haskell code to generate new singletons.

`Data.Singletons.Prelude` re-exports `Data.Singletons` along with singleton
definitions for various Prelude types. This module is intended to export
those definitions that are exported by the real `Prelude`.

There are several modules that echo standard modules. For example,
`Data.Singletons.Maybe` exports singleton definitions for `Data.Maybe`.
These modules are: `List` (many definitions are missing), `Bool`,
`Maybe`, `Either`, `Tuple`.

`Data.Singletons.Eq` and `Data.Singletons.Decide` export type classes for
Boolean and propositional equality, respectively.

`Data.Singletons.TypeLits` exports definitions for working with `GHC.TypeLits`.
In GHC 7.6.3, `Data.Singletons.TypeLits` defines and exports `KnownNat` and
`KnownSymbol`, which are part of `GHC.TypeLits` in GHC 7.8. This makes cross-version
support a little easier.

`Data.Singletons.Void` exports a `Void` type, shamelessly copied from
Edward Kmett's `void` package, but without the great many package dependencies
in `void`.

`Data.Singletons.Types` exports a few type-level definitions that are in
`base` for GHC 7.8, but not in GHC 7.6.3. By importing this package, users
of both GHC versions can access these definitions.

Functions to generate singletons
--------------------------------

The top-level functions used to generate singletons are documented in the
`Data.Singletons.TH` module. The most common case is just calling `singletons`,
which I'll describe here:

    singletons :: Q [Dec] -> Q [Dec]

Generates singletons from the definitions given. Because singleton generation
requires promotion, this also promotes all of the definitions given to the
type level.

To use:
    $(singletons [d|
      data Nat = Zero | Succ Nat
      pred :: Nat -> Nat
      pred Zero = Zero
      pred (Succ n) = n
      |])

Definitions used to support singletons
--------------------------------------

Please refer to the paper cited above for a more in-depth explanation of these
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
`SEq`. See the `Data.Singletons.Eq` module for more information.

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


On names
--------

The singletons library has to produce new names for the new constructs it
generates. Here are some examples showing how this is done:

original datatype: `Nat`  
promoted kind: `Nat`  
singleton type: `SNat` (which is really a synonym for `Sing`)  

original datatype: `:/\:`  
promoted kind: `:/\:`  
singleton type: `:%/\:`  

original constructor: `Zero`  
promoted type: `'Zero` (you can use `Zero` when unambiguous)  
singleton constructor: `SZero`  

original constructor: `:+:`  
promoted type: `':+:`  
singleton constructor: `:%+:`  

original value: `pred`  
promoted type: `Pred`  
singleton value: `sPred`  

original value: `+`  
promoted type: `:+`  
singleton value: `%:+`  


Special names
-------------

There are some special cases:

original datatype: `[]`  
singleton type: `SList`

original constructor: `[]`  
singleton constructor: `SNil`

original constructor: `:`  
singleton constructor: `SCons`

original datatype: `(,)`  
singleton type: `STuple2`

original constructor: `(,)`  
singleton constructor: `STuple2`

All tuples (including the 0-tuple, unit) are treated similarly.

original value: `undefined`  
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
* !, ~, and _ patterns
* aliased patterns (except at top-level)
* lists
* (+) sections
* (x +) sections
* undefined
* error
* deriving Eq
* class constraints
* literals (for `Nat` and `Symbol`)

The following constructs will be coming soon:

* unboxed tuples
* records
* scoped type variables
* overlapping patterns
* pattern guards
* (+ x) sections
* case
* let
* list comprehensions
* lambda expressions
* do
* arithmetic sequences

As described briefly in the paper, the singletons generation mechanism does not
currently work for higher-order datatypes (though higher-order functions are
just peachy). So, if you have a declaration such as

    data Foo = Bar (Bool -> Maybe Bool)

its singleton will not work correctly. It turns out that getting this to work
requires fairly thorough changes to the whole singleton generation scheme.
Please shout (to eir@cis.upenn.edu) if you have a compelling use case for this
and I can take a look at it. No promises, though.

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

Changes from earlier versions
-----------------------------

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