module Data.Singletons.TH.Syntax.LocalVar
  ( LocalVar
  , foldTypeLocalVars
  , localVarNoKind
  , localVarToTvb
  , localVarToType
  , localVarToTypeArg
  ) where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax

-- | A local variable that is captured in a lambda-lifted type family. (See
-- @Note [Tracking local variables]@ in "Data.Singletons.TH.Promote.Monad" for
-- an explanation of how lambda lifting works.) A 'LocalVar' consists of:
--
-- * A 'Name' that corresponds to the promoted, type-level version of a
--   term-level variable name.
--
-- * An optional kind (in the form of @'Maybe' 'DKind'@). When the kind of a
--   local variable is known, we can use it to generate code with more precise
--   kind information. See @Note [Local variables and kind information]@.
--
-- A 'LocalVar' is very close in design to a 'DTyVarBndrUnit', as both contain
-- 'Name's and optional 'DKind's. We use a separate 'LocalVar' type to represent
-- local variables because 'LocalVar's can occur both in binding and argument
-- positions in generated code (see @Note [Local variables and kind information]
-- (Wrinkle: Binding positions versus argument positions)@), and using
-- 'DTyVarBndrUnit's to represent type arguments feels somewhat awkward.
type LocalVar = (Name, Maybe DKind)

-- | Apply a 'DType' to a list of 'LocalVar' arguments. Because these
-- 'LocalVar's occur in argument positions, they will not contain any kind
-- information. See @Note [Local variables and kind information] (Wrinkle:
-- Binding positions versus argument positions)@.
foldTypeLocalVars :: DType -> [LocalVar] -> DType
foldTypeLocalVars ty = applyDType ty . map localVarToTypeArg

-- | Construct a 'LocalVar' with no kind information.
localVarNoKind :: Name -> LocalVar
localVarNoKind nm = (nm, Nothing)

-- | Convert a 'LocalVar' used in a binding position to a 'DTyVarBndr' using the
-- supplied @flag@. Because this is used in a binding position, we include kind
-- information (if available) in the 'DTyVarBndr'. See @Note [Local variables
-- and kind information] (Wrinkle: Binding positions versus argument
-- positions)@.
localVarToTvb :: flag -> LocalVar -> DTyVarBndr flag
localVarToTvb flag (nm, mbKind) =
  case mbKind of
    Nothing   -> DPlainTV nm flag
    Just kind -> DKindedTV nm flag kind

-- | Convert a 'LocalVar' used in an argument position to a 'DType'. Because
-- this is used in an argument positions, it will not kind any kind information.
-- See @Note [Local variables and kind information] (Wrinkle: Binding positions
-- versus argument positions)@.
localVarToType :: LocalVar -> DType
localVarToType (local_nm, _) = DVarT local_nm

-- | Convert a 'LocalVar' used in an argument position to a 'DTypeArg'. Because
-- this is used in an argument positions, it will not kind any kind information.
-- See @Note [Local variables and kind information] (Wrinkle: Binding positions
-- versus argument positions)@.
localVarToTypeArg :: LocalVar -> DTypeArg
localVarToTypeArg = DTANormal . localVarToType

{-
Note [Local variables and kind information]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following function, which we want to promote to the type level:

  f :: forall a. a -> a
  f x = y
    where
      y = y

Per Note [Tracking local variables]@ in Data.Singletons.TH.Promote.Monad, we
observe that `y` closes over two local variables, `a` and `x`. A naïve attempt
at promoting this code would result in generate code that looks something like
this:

  type F :: forall a. a -> a
  type family F x where
    F @a x = LetY a x

  type family LetY a x where
    LetY a x = x

In today's GHC, this kind-checks. However, the generated code is somewhat
unsatisfying, as GHC infers a kind for LetY that is way more general than it
should be:

  LetY :: forall k1 k2. k1 -> k2 -> k2

Note that this inferred kind says nothing about the relationship between the
first and second visible arguments. In today's GHC, this works because the body
of LetY requires the kind of the second visible argument to be equal to the
first visible argument in order to match. It would be as if you had written
this code:

  type LetY :: forall k1 k2. k1 -> k2 -> k2
  type family LetY a x where
    LetY @Type @a (a :: Type) (x :: a) = x

This sort of thing will cause problems once
https://gitlab.haskell.org/ghc/ghc/-/issues/23515 is implemented. As such, we
should strive to do better.

Fortunately, there is a relatively easy fix that works well here. We observe
that we know what the kind of `x` just by looking at the syntax of `f`'s
definition, as we can pair up the `x` argument with the `a` argument type in
`f`'s type. As such, we can remember that `x :: a` and instead generate:

  type family LetY a (x :: a) where
    LetY a x = x

Now GHC infers exactly the kind we'd want for LetY:

  LetY :: forall a -> a -> a

On the implementation side, we achive this by tracking optional kind
information in each LocalVar (in the form of a `Maybe DKind` field). A key
place in the code where kind information is propagated through to LocalVars is
the `promotePat` function in Data.Singletons.TH.Promote, which takes a `Maybe
DKind` field that describes the kind of the pattern being promoted (if it is
known). This allows recording the types of DSigP patterns (e.g., the `y :: b`
pattern in `g (y :: b) = Nothing :: Maybe b`), as well as recording the kinds
of variable patterns whose types are described by top-level top signatures
(e.g., the `x` pattern in the `f x = y` example above).

This approach has its limitations. Consider this slightly more complicated
example:

  f' :: forall a. [a] -> Maybe (a, [a])
  f' [] = Nothing
  f' (x:xs) = y
    where
      y = Just (x, xs)

Note that the patterns for the arguments to `f'` aren't bare variables this
time, but rather constructor patterns. As such, we don't know the types of `x`
and `xs` just by looking at the syntax of `f'`. Instead, we'd have to do some
more clever analysis to conclude that `x :: a` and `xs :: [a]`. This is perhaps
doable, but it would require something akin to implementing type inference in
Template Haskell, which is a direction of travel that I am reluctant to go
down.

As such, we do not record the types of the `x` or `xs` variables in this
example, meaning that we promote `f'` to the following:

  type F' :: forall a. [a] -> Maybe (a, [a])
  type family F' l where
    F' @a '[] = Nothing
    F' @a (x:xs) = LetY a x xs

  type family LetY a x xs where
    LetY a x xs = Just '(x, xs)

And GHC will infer an overly polymorphic kind for LetY:

  LetY :: k1 -> k2 -> k3 -> Maybe (k2, k3)

If this proves to be troublesome in the future, we could consider refining this
approach. It is also worth nothing that in the event that singletons-th
generates a local definition with an overly polymorphic kind, one can always
constrain the kind by inserting more pattern signatures. For instance, if you
redefine `f'` to be the following:

  f' :: forall a. [a] -> Maybe (a, [a])
  f' [] = Nothing
  f' ((x :: a) : (xs :: [a])) = y -- This line now has pattern signatures
    where
      y = Just (x, xs)

Then singletons-th will now realize what the kinds of `x` and `xs` are, and it
will generate code for `LetY` that uses these kinds.

-----
-- Wrinkle: Binding positions versus argument positions
-----

Although we track the kinds of local variables throughout promotion, we don't
want to necessarily generate code involving the kind in all circumstances.
Consider this example:

fNoScope :: a -> a
fNoScope x = y
  where
    y = x

`fNoScope` is like `f` above, except that `a` does not scope over the body of
the function due to the lack of an outermost `forall` in the type signature. On
the other hand, `x` /does/ scope over the body, so we close over `x` when
lambda lifting `y`. Moreover, we know that the type of `x` is `a`. However, we
must be careful not to promote `fNoScope` to the following:

  type FNoScope :: a -> a
  type family FNoScope x where
    FNoScope x = LetY (x :: a)

  type family LetY (x :: a) where
    LetY (x :: a) = x

The problem with this code lies here:

    FNoScope x = LetY (x :: a)

Note that `a` is not in scope in this line! Even though we know that `x :: a`,
that doesn't mean that we can unconditionally generate the code `x :: a` in all
places, since `a` may not be in scope in all places. Of course, we /do/ want to
generate `x :: a` in this line:

  type family LetY (x :: a) where

The distinction between the two lines is one of binding positions versus
argument positions. In the former case, `x` occurs as an argument to a `LetY`
application, whereas in the latter case, `x` occurs as a type variable binder
when defining `LetY`. In binding positions such as these, `x :: a` will
implicitly quantify `a`, so it is fine to unconditionally use `x :: a` in these
positions. Implicit quantification does not occur in argument positions,
however, so we leave out the `:: a` kind signature in these positions. (This is
perfectly fine to do, since `x` will be bound somewhere else, and that binder
will include the `a` kind information.)

Implementation-wise, the difference between these two positions is embodied in
the `localVarToTvb` function (for converting `LocalVar`s to binding positions)
and the `localVarToType` function (for converting `LocalVar`s to argument
positions). Note that the derived functions `localVarToTypeArg` and
`foldTypeLocalVars` also treat `LocalVar`s as argument positions.
-}
