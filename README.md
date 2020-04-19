singletons 2.8
==============

[![Hackage](https://img.shields.io/hackage/v/singletons.svg)](http://hackage.haskell.org/package/singletons)
[![Build Status](https://travis-ci.org/goldfirere/singletons.svg?branch=master)](https://travis-ci.org/goldfirere/singletons)

This is the README file for the `singletons` library. This file contains all the
documentation for the definitions and functions in the library.

The `singletons` library was written by Richard Eisenberg (<rae@cs.brynmawr.edu>) and
with significant contributions by Jan Stolarek (<jan.stolarek@p.lodz.pl>) and
Ryan Scott (<ryan.gl.scott@gmail.com>). There
are two papers that describe the library. Original one, _Dependently typed
programming with singletons_, is available
[here](https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf) and will
be referenced in this documentation as the "singletons paper". A follow-up
paper, _Promoting Functions to Type Families in Haskell_, is available
[here](https://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf)
and will be referenced in this documentation as the
"promotion paper".

Ryan Scott (<ryan.gl.scott@gmail.com>) is the active maintainer.

Purpose of the `singletons` library
-----------------------------------

The library contains a definition of _singleton types_, which allow programmers
to use dependently typed techniques to enforce rich constraints among the types
in their programs. See the singletons paper for a more thorough introduction.

The package also allows _promotion_ of term-level functions to type-level
equivalents and _singling_ functions to dependently typed equivalents.
Accordingly, it exports a Prelude of promoted and singled
functions, mirroring functions and datatypes found in the `Prelude`, `Data.Bool`,
`Data.Maybe`, `Data.Either`, `Data.Tuple` and `Data.List`. See the promotion
paper for a more thorough introduction.

[This blog series](https://blog.jle.im/entry/introduction-to-singletons-1.html),
authored by Justin Le, offers a tutorial for this library that assumes no
knowledge of dependent types.

Compatibility
-------------

The `singletons` library requires GHC 8.10.1 or greater. Any code that uses the
singleton generation primitives needs to enable a long list of GHC
extensions. This list includes, but is not necessarily limited to, the
following:

* `DataKinds`
* `DefaultSignatures`
* `EmptyCase`
* `ExistentialQuantification`
* `FlexibleContexts`
* `FlexibleInstances`
* `GADTs`
* `InstanceSigs`
* `KindSignatures`
* `NoCUSKs`
* `NoStarIsType`
* `PolyKinds`
* `RankNTypes`
* `ScopedTypeVariables`
* `StandaloneKindSignatures`
* `TemplateHaskell`
* `TypeApplications`
* `TypeFamilies`
* `TypeOperators`
* `UndecidableInstances`

In particular, `NoStarIsType` is needed to use the `*` type family from the
`PNum` class because with `StarIsType` enabled, GHC thinks `*` is a synonym
for `Type`.

You may also want to consider toggling various warning flags:

* `-Wno-redundant-constraints`.
  The code that `singletons` generates uses redundant constraints, and there
  seems to be no way, without a large library redesign, to avoid this.
* `-fenable-th-splice-warnings`.
  By default, GHC does not run pattern-match coverage checker warnings on code
  inside of Template Haskell quotes. This is an extremely common thing to do
  in `singletons`, so you may consider opting in to these warnings.

Modules for singleton types
---------------------------

`Data.Singletons` exports all the basic singletons definitions. Import this
module if you are not using Template Haskell and wish only to define your
own singletons.

`Data.Singletons.TH` exports all the definitions needed to use the Template
Haskell code to generate new singletons.

`Data.Singletons.Prelude` re-exports `Data.Singletons` along with singleton
definitions for various `Prelude` types. This module provides promoted and
singled equivalents of functions from the real `Prelude`.
Note that not all functions from original `Prelude` could be promoted or
singled.

`Data.Singletons.Prelude.*` modules provide promoted and singled equivalents of
definitions found in several commonly used `base` library modules, including
(but not limited to) `Data.Bool`, `Data.Maybe`, `Data.Either`, `Data.List`,
`Data.Tuple`, `Data.Void` and `GHC.Base`. We also provide promoted and singled
versions of common type classes, including (but not limited to) `Eq`, `Ord`,
`Show`, `Enum`, and `Bounded`.

`Data.Singletons.Decide` exports type classes for propositional equality.

`Data.Singletons.TypeLits` exports definitions for working with `GHC.TypeLits`.

Functions to generate singletons
--------------------------------

The top-level functions used to generate promoted or singled definitions are
documented in the `Data.Singletons.TH` module. The most common case is just
calling `singletons`, which I'll describe here:

```haskell
singletons :: Q [Dec] -> Q [Dec]
```

This function generates singletons from the definitions given. Because
singleton generation requires promotion, this also promotes all of the
definitions given to the type level.

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

```haskell
type Sing :: k -> Type
type family Sing
```

The type family of singleton types. A new instance of this type family is
generated for every new singleton type.

```haskell
class SingI a where
  sing :: Sing a
```

A class used to pass singleton values implicitly. The `sing` method produces
an explicit singleton value.

```haskell
type SomeSing :: Type -> Type
data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k
```

The `SomeSing` type wraps up an _existentially-quantified_ singleton. Note that
the type parameter `a` does not appear in the `SomeSing` type. Thus, this type
can be used when you have a singleton, but you don't know at compile time what
it will be. `SomeSing Thing` is isomorphic to `Thing`.

```haskell
type SingKind :: Type -> Constraint
class SingKind k where
  type Demote k :: *
  fromSing :: Sing (a :: k) -> Demote k
  toSing   :: Demote k -> SomeSing k
```

This class is used to convert a singleton value back to a value in the
original, unrefined ADT. The `fromSing` method converts, say, a
singleton `Nat` back to an ordinary `Nat`. The `toSing` method produces
an existentially-quantified singleton, wrapped up in a `SomeSing`.
The `Demote` associated
kind-indexed type family maps the kind `Nat` back to the type `Nat`.

```haskell
type SingInstance :: k -> Type
data SingInstance a where
  SingInstance :: SingI a => SingInstance a
singInstance :: Sing a -> SingInstance a
```

Sometimes you have an explicit singleton (a `Sing`) where you need an implicit
one (a dictionary for `SingI`). The `SingInstance` type simply wraps a `SingI`
dictionary, and the `singInstance` function produces this dictionary from an
explicit singleton. The `singInstance` function runs in constant time, using
a little magic.


Equality classes
----------------

There are two different notions of equality applicable to singletons: Boolean
equality and propositional equality.

* Boolean equality is implemented in the type family `(==)` (in the `PEq`
  class) and the `(%==`) method (in the `SEq` class).
  See the `Data.Singletons.Prelude.Eq` module for more information.

* Propositional equality is implemented through the constraint `(~)`, the type
  `(:~:)`, and the class `SDecide`. See modules `Data.Type.Equality` and
  `Data.Singletons.Decide` for more information.

Which one do you need? That depends on your application. Boolean equality has
the advantage that your program can take action when two types do _not_ equal,
while propositional equality has the advantage that GHC can use the equality
of types during type inference.

Instances of `SEq`, `SDecide`, `TestEquality`, and `TestCoercion` are generated
when `singletons` is called on a datatype that has `deriving Eq`. You can also
generate these instances directly through functions exported from
`Data.Singletons.TH`.


`Show` classes
--------------

Promoted and singled versions of the `Show` class (`PShow` and `SShow`,
respectively) are provided in the `Data.Singletons.Prelude.Show` module. In
addition, there is a `ShowSing` constraint synonym provided in the
`Data.Singletons.ShowSing` module:

```haskell
type ShowSing :: Type -> Constraint
type ShowSing k = (forall z. Show (Sing (z :: k)) -- Approximately
```

This facilitates the ability to write `Show` instances for `Sing` instances.

What distinguishes all of these `Show`s? Let's use the `False` constructor as
an example. If you used the `PShow Bool` instance, then the output of calling
`Show_` on `False` is `"False"`, much like the value-level `Show Bool` instance
(similarly for the `SShow Bool` instance). However, the `Show (Sing (z :: Bool))`
instance (i.e., `ShowSing Bool`) is intended for printing the value of the
_singleton_ constructor `SFalse`, so calling `show SFalse` yields `"SFalse"`.

Instance of `PShow`, `SShow`, and `Show` (for the singleton type) are generated
when `singletons` is called on a datatype that has `deriving Show`. You can also
generate these instances directly through functions exported from
`Data.Singletons.TH`.

A promoted and singled `Show` instance is provided for `Symbol`, but it is only
a crude approximation of the value-level `Show` instance for `String`. On the
value level, showing `String`s escapes special characters (such as double
quotes), but implementing this requires pattern-matching on character literals,
something which is currently impossible at the type level. As a consequence, the
type-level `Show` instance for `Symbol`s does not do any character escaping.

Errors
------

The `singletons` library provides two different ways to handle errors:

* The `Error` type family, from `Data.Singletons.TypeLits`:

  ```haskell
  type Error :: a -> k
  type family Error str where {}
  ```

  This is simply an empty, closed type family, which means that it will fail
  to reduce regardless of its input. The typical use case is giving it a
  `Symbol` as an argument, so that something akin to
  `Error "This is an error message"` appears in error messages.
* The `TypeError` type family, from `Data.Singletons.TypeError`. This is a
  drop-in replacement for `TypeError` from `GHC.TypeLits` which can be used
  at both the type level and the value level (via the `typeError` function).

  Unlike `Error`, `TypeError` will result in an actual compile-time error
  message, which may be more desirable depending on the use case.

Pre-defined singletons
----------------------

The `singletons` library defines a number of singleton types and functions
by default. These include (but are not limited to):

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
definitions. Almost all Haskell source constructs are supported -- see the
"Supported Haskell constructs" section of this README for a full list.

Promoted definitions are usually generated by calling the `promote` function:

```haskell
$(promote [d|
  data Nat = Zero | Succ Nat
  pred :: Nat -> Nat
  pred Zero = Zero
  pred (Succ n) = n
  |])
```

Every promoted function and data constructor definition comes with a set of
so-called _defunctionalization symbols_. These are required to represent
partial application at the type level. For more information, refer to the
"Promotion and partial application" section below.

Users also have access to `Data.Singletons.Prelude` and its submodules (e.g.,
`Base`, `Bool`, `Either`, `List`, `Maybe` and `Tuple`). These provide promoted
versions of function found in GHC's `base` library.

Note that GHC resolves variable names in Template Haskell quotes. You cannot
then use an undefined identifier in a quote, making idioms like this not
work:

```haskell
type family Foo a where ...
$(promote [d| ... foo x ... |])
```

In this example, `foo` would be out of scope.

Refer to the promotion paper for more details on function promotion.

Promotion and partial application
---------------------------------

Promoting higher-order functions proves to be surprisingly tricky. Consider
this example:

```hs
$(promote [d|
  map :: (a -> b) -> [a] -> [b]
  map _ []     = []
  map f (x:xs) = f x : map f xs
  |])
```

A naïve attempt to promote `map` would be:

```hs
type Map :: (a -> b) -> [a] -> [b]
type family Map f xs where
  Map _ '[]    = '[]
  Map f (x:xs) = f x : Map f xs
```

While this compiles, it is much less useful than we would like. In particular,
common idioms like `Map Id xs` will not typecheck, since GHC requires that all
invocations of type families be fully saturated. That is, the use of `Id` in
`Map Id xs` is rejected since it is not applied to one argument, which the
number of arguments that `Id` was defined with. For more information on this
point, refer to the promotion paper.

Not having the ability to partially apply functions at the type level is rather
painful, so we do the next best thing: we _defunctionalize_ all promoted
functions so that we can emulate partial application. For example, if one were
to promote the `id` function:

```hs
$(promote [d|
  id :: a -> a
  id x = x
  |]
```

Then in addition to generating the promoted `Id` type family, two
defunctionalization symbols will be generated:

```hs
type IdSym0 :: a ~> a
data IdSym0 x

type IdSym1 :: a -> a
type family IdSym1 x where
  IdSym1 x = Id x
```

In general, a function that accepts N arguments generates N+1 defunctionalization
symbols when promoted.

`IdSym1` is a _fully saturated_ defunctionalization symbol and is usually only
needed when generating code through the Template Haskell machinery. `IdSym0`
is more interesting: it has the kind `a ~> a`, which has a special arrow type
`(~>)`. Defunctionalization symbols using the `(~>)` kind are type-level
constants that can be "applied" using a special `Apply` type family:

```hs
type Apply :: (a ~> b) -> a -> b
type family Apply f x
```

Every defunctionalization symbol comes with a corresponding `Apply` instance
(except for fully saturated defunctionalization symbols). For instance, here
is the `Apply` instance for `IdSym0`:

```hs
type instance Apply IdSym0 x = Id x
```

The `(~>)` kind is used when promoting higher-order functions so that partially
applied arguments can be passed to them. For instance, here is our final attempt
at promoting `map`:

```hs
type Map :: (a ~> b) -> [a] -> [b]
type family Map f xs where
  Map _ '[]    = '[]
  Map f (x:xs) = Apply f x : Map f xs
```

Now `map id xs` can be promoted to `Map IdSym0 xs`, which typechecks without issue.

## Defunctionalizing existing type families

The most common way to defunctionalize functions is by promoting them with the
Template Haskell machinery. One can also defunctionalize existing type families,
however, by using `genDefunSymbols`. For example:

```hs
type MyTypeFamily :: Nat -> Bool
type family MyTypeFamily n

$(genDefunSymbols [''MyTypeFamily])
```

This can be especially useful if `MyTypeFamily` needs to be implemented by
hand. Be aware of the following design limitations of `genDefunSymbols`:

* `genDefunSymbols` only works for type-level declarations. Namely, it only
  works when given the names of type classes, type families, type synonyms,
  or data types. Attempting to pass the name of a term level function,
  class method, data constructor, or record selector will throw an error.
* Passing the name of a data type to `genDefunSymbols` will cause its
  data constructors to be defunctionalized but _not_ its record selectors.
* Passing the name of a type class to `genDefunSymbols` will cause the
  class itself to be defunctionalized, but /not/ its associated type families
  or methods.

Note that the limitations above reflect the current design of
`genDefunSymbols`. As a result, they are subject to change in the future.

## Defunctionalization and visible dependent quantification

Unlike most other parts of `singletons`, which disallow visible dependent
quantification (VDQ), `genDefunSymbols` has limited support for VDQ.
Consider this example:

```hs
type MyProxy :: forall (k :: Type) -> k -> Type
type family MyProxy k (a :: k) :: Type where
  MyProxy k (a :: k) = Proxy a

$(genDefunSymbols [''MyProxy])
```

This will generate the following defunctionalization symbols:

```hs
type MyProxySym0 ::              Type  ~> k ~> Type
type MyProxySym1 :: forall (k :: Type) -> k ~> Type
type MyProxySym2 :: forall (k :: Type) -> k -> Type
```

Note that `MyProxySym0` is a bit more general than it ought to be, since
there is no dependency between the first kind (`Type`) and the second kind
(`k`). But this would require the ability to write something like this:

```hs
type MyProxySym0 :: forall (k :: Type) ~> k ~> Type
```

This currently isn't possible. So for the time being, the kind of
`MyProxySym0` will be slightly more general, which means that under rare
circumstances, you may have to provide extra type signatures if you write
code which exploits the dependency in `MyProxy`'s kind.

Classes and instances
---------------------

This is best understood by example. Let's look at a stripped down `Ord`:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)     :: a -> a -> Bool
  x < y = case x `compare` y of
            LT -> True
	    EQ -> False
	    GT -> False
```

This class gets promoted to a "kind class" thus:

```haskell
class PEq a => POrd a where
  type Compare (x :: a) (y :: a) :: Ordering
  type (<)     (x :: a) (y :: a) :: Bool
  type x < y = ... -- promoting `case` is yucky.
```

Note that default method definitions become default associated type family
instances. This works out quite nicely.

We also get this singleton class:

```haskell
class SEq a => SOrd a where
  sCompare :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (Compare x y)
  (%<)     :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x < y)

  default (%<) :: forall (x :: a) (y :: a).
                  ((x < y) ~ {- RHS from (<) above -})
		=> Sing x -> Sing y -> Sing (x < y)
  x %< y = ...  -- this is a bit yucky too
```

Note that a singled class needs to use `default` signatures, because
type-checking the default body requires that the default associated type
family instance was used in the promoted class. The extra equality constraint
on the default signature asserts this fact to the type checker.

Instances work roughly similarly.

```haskell
instance Ord Bool where
  compare False False = EQ
  compare False True  = LT
  compare True  False = GT
  compare True  True  = EQ

instance POrd Bool where
  type Compare 'False 'False = 'EQ
  type Compare 'False 'True  = 'LT
  type Compare 'True  'False = 'GT
  type Compare 'True  'True  = 'EQ

instance SOrd Bool where
  sCompare :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (Compare x y)
  sCompare SFalse SFalse = SEQ
  sCompare SFalse STrue  = SLT
  sCompare STrue  SFalse = SGT
  sCompare STrue  STrue  = SEQ
```

The only interesting bit here is the instance signature. It's not necessary
in such a simple scenario, but more complicated functions need to refer to
scoped type variables, which the instance signature can bring into scope.
The defaults all just work.

On names
--------

The `singletons` library has to produce new names for the new constructs it
generates. Here are some examples showing how this is done:

1. original datatype: `Nat`

   promoted kind: `Nat`

   singleton type: `SNat` (which is really a synonym for `Sing`)


2. original datatype: `/\`

   promoted kind: `/\`

   singleton type: `%/\`


3. original constructor: `Succ`

   promoted type: `'Succ` (you can use `Succ` when unambiguous)

   singleton constructor: `SSucc`

   symbols: `SuccSym0`, `SuccSym1`


4. original constructor: `:+:`

   promoted type: `':+:`

   singleton constructor: `:%+:`

   symbols: `:+:@#@$`, `:+:@#@$$`, `:+:@#@$$$`


5. original value: `pred`

   promoted type: `Pred`

   singleton value: `sPred`

   symbols: `PredSym0`, `PredSym1`


6. original value: `+`

   promoted type: `+`

   singleton value: `%+`

   symbols: `+@#@$`, `+@#@$$`, `+@#@$$$`


7. original class: `Num`

   promoted class: `PNum`

   singleton class: `SNum`


8. original class: `~>`

   promoted class: `#~>`

   singleton class: `%~>`


Special names
-------------

There are some special cases, listed below (with asterisks\* denoting special
treatment):

1. original datatype: `[]`

   promoted kind: `[]`

   singleton type\*: `SList`


2. original constructor: `[]`

   promoted type: `'[]`

   singleton constructor\*: `SNil`

   symbols\*: `NilSym0`


3. original constructor: `:`

   promoted type: `':`

   singleton constructor\*: `SCons`

   symbols: `:@#@$`, `:@#@$$`, `:@#@$$$`


4. original datatype: `(,)`

   promoted kind: `(,)`

   singleton type\*: `STuple2`


5. original constructor: `(,)`

   promoted type: `'(,)`

   singleton constructor\*: `STuple2`

   symbols\*: `Tuple2Sym0`, `Tuple2Sym1`, `Tuple2Sym2`

   All tuples (including the 0-tuple, unit) are treated similarly.


6. original value: `___foo`

   promoted type\*: `US___foo` ("`US`" stands for "underscore")

   singleton value\*: `___sfoo`

   symbols\*: `US___fooSym0`

   All functions that begin with leading underscores are treated similarly.

If desired, you can pick your own naming conventions by using the
`Data.Singletons.TH.Options` module. Here is an example of how this module can
be used to prefix a singled data constructor with `MyS` instead of `S`:

```hs
import Control.Monad.Trans.Class
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Language.Haskell.TH (Name, mkName, nameBase)

$(let myPrefix :: Name -> Name
      myPrefix name = mkName ("MyS" ++ nameBase name) in

      withOptions defaultOptions{singledDataConName = myPrefix} $
      singletons $ lift [d| data T = MkT |])
```

Supported Haskell constructs
----------------------------

## Full support

The following constructs are fully supported:

* variables
* tuples
* constructors
* if statements
* infix expressions and types
* `_` patterns
* aliased patterns
* lists (including list comprehensions)
* `do`-notation
* sections
* undefined
* error
* class constraints (though these sometimes fail with `let`, `lambda`, and `case`)
* literals (for `Nat` and `Symbol`), including overloaded number literals
* unboxed tuples (which are treated as normal tuples)
* pattern guards
* case
* let
* lambda expressions
* `!` and `~` patterns (silently but successfully ignored during promotion)
* class and instance declarations
* signatures (e.g., `(x :: Maybe a)`) in expressions
* `InstanceSigs`

## Partial support

The following constructs are partially supported:

* `deriving`
* finite arithmetic sequences
* records
* signatures (e.g., `(x :: Maybe a)`) in patterns
* functional dependencies
* type families

See the following sections for more details.

### `deriving`

`singletons` is slightly more conservative with respect to `deriving` than GHC is.
The only classes that `singletons` can derive without an explicit deriving
strategy are the following stock classes:

* `Eq`
* `Ord`
* `Show`
* `Bounded`
* `Enum`
* `Functor`
* `Foldable`
* `Traversable`

To do anything more exotic, one must explicitly indicate one's intentions by
using the `DerivingStrategies` extension. `singletons` fully supports the
`anyclass` strategy as well as the `stock` strategy (at least, for the classes
listed above). `singletons` does not support the `newtype` or `via` strategies,
as there is no equivalent of `coerce` at the type level.

### Finite arithmetic sequences

`singletons` has partial support for arithmetic sequences (which desugar to
methods from the `Enum` class under the hood). _Finite_ sequences (e.g.,
[0..42]) are fully supported. However, _infinite_ sequences (e.g., [0..]),
which desugar to calls to `enumFromTo` or `enumFromThenTo`, are not supported,
as these would require using infinite lists at the type level.

### Records

Record selectors are promoted to top-level functions, as there is no record
syntax at the type level. Record selectors are also singled to top-level
functions because embedding records directly into singleton data constructors
can result in surprising behavior (see
[this bug report](https://github.com/goldfirere/singletons/issues/364) for more
details on this point). TH-generated code is not affected by this limitation
since `singletons` desugars away most uses of record syntax. On the other hand,
it is not possible to write out code like
`SIdentity { sRunIdentity = SIdentity STrue }` by hand.

### Signatures in patterns

`singletons` can promote basic pattern signatures, such as in the following
examples:

```hs
f :: forall a. a -> a
f (x :: a) = (x :: a)

g :: forall a. a -> a
g (x :: b) = (x :: b) -- b is the same as a
```

What does /not/ work are more advanced uses of pattern signatures that take
advantage of the fact that type variables in pattern signatures can alias other
types. Here are some examples of functions that one cannot promote:

* ```hs
  h :: a -> a -> a
  h (x :: a) (_ :: b) = x
  ```

  This typechecks by virtue of the fact that `b` aliases `a`. However, the same
  trick does not work when `h` is promoted to a type family, as a type family
  would consider `a` and `b` to be distinct type variables.
* ```hs
  i :: Bool -> Bool
  i (x :: a) = x
  ```

  This typechecks by virtue of the fact that `a` aliases `Bool`. Again, this
  would not work at the type level, as a type family would consider `a` to be
  a separate type from `Bool`.

### Functional dependencies

Inference dependent on functional dependencies is unpredictably bad. The
problem is that a use of an associated type family tied to a class with
fundeps doesn't provoke the fundep to kick in. This is GHC's problem, in
the end.

### Type families

Promoting functions with types that contain type families is likely to fail due to
[GHC#12564](https://gitlab.haskell.org/ghc/ghc/issues/12564).
Note that promoting type family _declarations_ is fine
(and often desired, since that produces defunctionalization symbols for them).

## Support for promotion, but not singling

The following constructs are supported for promotion but not singleton generation:

* data constructors with contexts
* overlapping patterns
* `GADTs`
* instances of poly-kinded type classes

See the following sections for more details.

### Data constructors with contexts

For example, the following datatype does not single:

```haskell
data T a where
  MkT :: Show a => a -> T a
```

Constructors like these do not interact well with the current design of the
`SingKind` class. But see
[this bug report](https://github.com/goldfirere/singletons/issues/150), which
proposes a redesign for `SingKind` (in a future version of GHC with certain
bugfixes) which could permit constructors with equality constraints.

### Overlapping patterns

Note that overlapping patterns are sometimes not obvious. For example, the
`filter` function does not single due to overlapping patterns:

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
```

Overlap is caused by `otherwise` catch-all guard, which is always true and thus
overlaps with `pred x` guard.

Another non-obvious source of overlapping patterns comes from partial pattern
matches in `do`-notation. For example:

```haskell
f :: [()]
f = do
  Just () <- [Nothing]
  return ()
```

This has overlap because the partial pattern match desugars to the following:

```haskell
f :: [()]
f = case [Nothing] of
      Just () -> return ()
      _ -> fail "Partial pattern match in do notation"
```

Here, it is more evident that the catch-all pattern `_` overlaps with the
one above it.

### `GADTs`

Singling GADTs is likely to fail due to the generated `SingKind` instances
not typechecking. (See
[#150](https://github.com/goldfirere/singletons/issues/150)).
However, one can often work around the issue by suppressing the generation
of `SingKind` instances by using custom `Options`. See the `T150` test case
for an example.

### Instances of poly-kinded type classes

Singling instances of poly-kinded type classes is likely to fail due to
[#358](https://github.com/goldfirere/singletons/issues/358).
However, one can often work around the issue by using `InstanceSigs`. For
instance, the following code will not single:

```haskell
class C (f :: k -> Type) where
  method :: f a

instance C [] where
  method = []
```

Adding a type signature for `method` in the `C []` is sufficient
to work around the issue, though:

```haskell
instance C [] where
  method :: [a]
  method = []
```

## Little to no support

The following constructs are either unsupported or almost never work:

* scoped type variables
* datatypes that store arrows, `Nat`, or `Symbol`
* rank-n types
* promoting `TypeRep`s
* `TypeApplications`

See the following sections for more details.

### Scoped type variables

Promoting functions that rely on the behavior of `ScopedTypeVariables` is very
tricky—see
[this GitHub issue](https://github.com/goldfirere/singletons/issues/433) for an
extended discussion on the topic. This is not to say that promoting functions
that rely on `ScopedTypeVariables` is guaranteed to fail, but it is rather
fragile. To demonstrate how fragile this is, note that the following function
will promote successfully:

```hs
f :: forall a. a -> a
f x = id x :: a
```

But this one will not:

```hs
g :: forall a. a -> a
g x = id (x :: a)
```

There are usually workarounds one can use instead of `ScopedTypeVariables`:

1. Use pattern signatures:

   ```hs
   g :: forall a. a -> a
   g (x :: a) = id (x :: a)
   ```
2. Use local definitions:

   ```hs
   g :: forall a. a -> a
   g x = id' a
     where
       id' :: a -> a
       id' x = x
   ```

### Arrows, `Nat`, `Symbol`, and literals

As described in the promotion paper, promotion of datatypes that store arrows is
currently impossible. So if you have a declaration such as

```haskell
data Foo = Bar (Bool -> Maybe Bool)
```

you will quickly run into errors.

Literals are problematic because we rely on GHC's built-in support, which
currently is limited. Functions that operate on strings will not work because
type level strings are no longer considered lists of characters. Functions
working over integer literals can be promoted by rewriting them to use
`Nat`. Since `Nat` does not exist at the term level, it will only be possible to
use the promoted definition, but not the original, term-level one.

For now, one way to work around this issue is to define data types that are
parameterized over the choice of arrow (or literal) type. The example below
demonstrates this workaround in the context of `Nat`s:

```hs
import Data.Kind
import Data.Singletons
import Data.Singletons.TypeLits
import Numeric.Natural

-- Base type
newtype Age' n = MkAge n
-- Term-level
type Age = Age' Natural
-- Type-level
type PAge = Age' Nat

type SAge :: PAge -> Type
data SAge age where
  SMkAge :: Sing n -> SAge (MkAge n)
type instance Sing = SAge

instance SingKind PAge where
  type Demote PAge = Age
  fromSing (SMkAge sn) = MkAge (fromSing sn)
  toSing (MkAge n) = withSomeSing n (SomeSing . SMkAge)
```

Note how the base type `Age'` is parameterized over which natural number type
to use. `Age` (intended to be used at the term level) uses
`Numeric.Natural.Natural`, whereas `PAge` (intended to be used at the type
level) uses `GHC.TypeNats.Nat`.

It should be emphasized that attempting to use `singletons` Template Haskell
machinery to promote code that uses the type `Age` is unlikely to work, since
`singletons` does not know about the relationship between `Age` and `PAge`.
As a result, you will likely have to write such code by hand. (See also
[this bug report](https://github.com/goldfirere/singletons/issues/450), which
proposes a way to generate some `Age`-related code automatically.)

A similar workaround exists for `Symbol`:

```hs
import Data.Text

-- Base type
newtype Message' s = MkMessage s
-- Term-level
type Message = Message' Text
-- Type-level
type PMessage = Message' Symbol
```

The workaround for arrow types is somewhat trickier since they have arguments:

```hs
-- Base type
newtype Function' p a b = MkFunction (p @@ a @@ b)
-- Term-level (i.e., MkFunction (a -> b))
type Function = Function' (TyCon2 (->))
-- Type-level (i.e., MkFunction (a ~> b))
type PFunction = Function' (~>@#@$)
```

### Rank-n types

`singletons` does not support type signatures that have higher-rank types.
More precisely, the only types that can be promoted or singled are
_vanilla_ types,  where a vanilla function type is a type that:

1. Only uses a `forall` at the top level, if used at all. That is to say, it
   does not contain any nested or higher-rank `forall`s.

2. Only uses a context (e.g., `c => ...`) at the top level, if used at all,
   and only after the top-level `forall` if one is present. That is to say,
   it does not contain any nested or higher-rank contexts.

3. Contains no visible dependent quantification.

### Promoting `TypeRep`s

The built-in Haskell promotion mechanism does not yet have a full story around
the kind `*` (the kind of types that have values). Ideally, promoting some form
of `TypeRep` would yield `*`, but the implementation of `TypeRep` would have to
be updated for this to really work out. In the meantime, users who wish to
experiment with this feature have two options:

1) The module `Data.Singletons.TypeRepTYPE` has all the definitions possible for
making `*` the promoted version of `TypeRep`, as `TypeRep` is currently implemented.
The singleton associated with `TypeRep` has one constructor:

    ```haskell
    type instance Sing @(TYPE rep) = TypeRep
    ```

    (Recall that `type * = TYPE LiftedRep`.) Note that any datatypes that store
`TypeRep`s will not generally work as expected; the built-in promotion
mechanism will not promote `TypeRep` to `*`.

2) The module `Data.Singletons.CustomStar` allows the programmer to define a subset
of types with which to work. See the Haddock documentation for the function
`singletonStar` for more info.

### `TypeApplications`

`singletons` currently cannot handle promoting or singling code that uses
`TypeApplications` syntax, so `singletons` will simply drop any visible type
applications. For example, `id @Bool True` will be promoted to `Id True` and
singled to `sId STrue`. See
[#378](https://github.com/goldfirere/singletons/issues/378) for a discussion
of how `singletons` may support `TypeApplications` in the future.

On the other hand, `singletons` does make an effort to preserve the order of
type variables when promoting and singling certain constructors. These include:

* Kind signatures of promoted top-level functions
* Type signatures of singled top-level functions
* Kind signatures of singled data type declarations
* Type signatures of singled data constructors
* Kind signatures of singled class declarations
* Type signatures of singled class methods

For example, consider this type signature:

```haskell
const2 :: forall b a. a -> b -> a
```

The promoted version of `const` will have the following kind signature:

```haskell
type Const2 :: forall b a. a -> b -> a
```

The singled version of `const2` will have the following type signature:

```haskell
sConst2 :: forall b a (x :: a) (y :: a). Sing x -> Sing y -> Sing (Const x y)
```

Therefore, writing `const2 @T1 @T2` works just as well as writing
`Const2 @T1 @T2` or `sConst2 @T1 @T2`, since the signatures for `const2`, `Const2`,
and `sConst2` all begin with `forall b a.`, in that order. Again, it is worth
emphasizing that the TH machinery does not support promoting or singling
`const2 @T1 @T2` directly, but you can write the type applications by hand if
you so choose.

`singletons` also has limited support for preserving the order of type variables
for the following constructs:

* Kind signatures of defunctionalization symbols.
  The order of type variables is only guaranteed to be preserved if:

  1. The thing being defunctionalized has a standalone type (or kind)
     signature.
  2. The type (or kind) signature of the thing being defunctionalized is
     a vanilla type. (See the "Rank-n types" section above for what "vanilla"
     means.)

  If either of these conditions do not hold, `singletons` will fall back to
  a slightly different approach to generating defunctionalization symbols that
  does *not* guarantee the order of type variables. As an example, consider the
  following example:

  ```haskell
  data T (x :: a) :: forall b. b -> Type
  $(genDefunSymbols [''T])
  ```

  The kind of `T` is `forall a. a -> forall b. b -> Type`, which is not
  vanilla. Currently, `singletons` will generate the following
  defunctionalization symbols for `T`:

  ```haskell
  data TSym0 :: a ~> b ~> Type
  data TSym1 (x :: a) :: b ~> Type
  ```

  In both symbols, the kind starts with `forall a b.` rather than quantifying
  the `b` after the visible argument of kind `a`. These symbols can still be
  useful even with this flaw, so `singletons` permits generating them
  regardless. Be aware of this drawback if you try doing something similar
  yourself!

* Kind signatures of promoted class methods.
  The order of type variables will often "just work" by happy coincidence, but
  there are some situations where this does not happen. Consider the following
  class:

  ```haskell
  class C (b :: Type) where
    m :: forall a. a -> b -> a
  ```

  The full type of `m` is `forall b. C b => forall a. a -> b -> a`, which binds
  `b` before `a`. This order is preserved when singling `m`, but *not* when
  promoting `m`. This is because the `C` class is promoted as follows:

  ```haskell
  class PC (b :: Type) where
    type M (x :: a) (y :: b) :: a
  ```

  Due to the way GHC kind-checks associated type families, the kind of `M` is
  `forall a b. a -> b -> a`, which binds `b` *after* `a`. Moreover, the
  `StandaloneKindSignatures` extension does not provide a way to explicitly
  declare the full kind of an associated type family, so this limitation is
  not easy to work around.

  The defunctionalization symbols for `M` will also follow a similar
  order of type variables:

  ```haskell
  type MSym0 :: forall a b. a ~> b ~> a
  type MSym1 :: forall a b. a -> b ~> a
  ```
