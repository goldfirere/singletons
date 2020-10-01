{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#else
{-# LANGUAGE TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the basic definitions to use singletons. See also
-- @Prelude.Singletons@ from the @singletons-base@
-- library, which re-exports this module alongside many singled definitions
-- based on the "Prelude".
--
-- You may also want to read
-- the original papers presenting this library, available at
-- <http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf>
-- and <http://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf>.
--
----------------------------------------------------------------------------

module Data.Singletons (
  -- * Main singleton definitions

  Sing, SLambda(..), (@@),

  SingI(..), SingKind(..),

  -- * Working with singletons
  KindOf, SameKind,
  SingInstance(..), SomeSing(..),
  singInstance, pattern Sing, withSingI,
  withSomeSing, pattern FromSing,
  singByProxy, demote,

  singByProxy#,
  withSing, singThat,

  -- ** @WrappedSing@
  WrappedSing(..), SWrappedSing(..), UnwrapSing,
  -- $SingletonsOfSingletons

  -- ** Defunctionalization
  TyFun, type (~>),
  TyCon1, TyCon2, TyCon3, TyCon4, TyCon5, TyCon6, TyCon7, TyCon8,
  Apply, type (@@),
#if __GLASGOW_HASKELL__ >= 806
  TyCon, ApplyTyCon, ApplyTyConAux1, ApplyTyConAux2,
#endif

  -- ** Defunctionalized singletons
  -- | When calling a higher-order singleton function, you need to use a
  -- @singFun...@ function to wrap it. See 'singFun1'.
  singFun1, singFun2, singFun3, singFun4, singFun5, singFun6, singFun7,
  singFun8,
  unSingFun1, unSingFun2, unSingFun3, unSingFun4, unSingFun5,
  unSingFun6, unSingFun7, unSingFun8,
  -- $SLambdaPatternSynonyms
  pattern SLambda2, applySing2,
  pattern SLambda3, applySing3,
  pattern SLambda4, applySing4,
  pattern SLambda5, applySing5,
  pattern SLambda6, applySing6,
  pattern SLambda7, applySing7,
  pattern SLambda8, applySing8,

  -- | These type synonyms are exported only to improve error messages; users
  -- should not have to mention them.
  SingFunction1, SingFunction2, SingFunction3, SingFunction4, SingFunction5,
  SingFunction6, SingFunction7, SingFunction8,

  -- * Auxiliary functions
  Proxy(..),

  -- * Defunctionalization symbols
  DemoteSym0, DemoteSym1,
  SameKindSym0, SameKindSym1, SameKindSym2,
  KindOfSym0, KindOfSym1,
  type (~>@#@$), type (~>@#@$$), type (~>@#@$$$),
  ApplySym0, ApplySym1, ApplySym2,
  type (@@@#@$), type (@@@#@$$), type (@@@#@$$$)
  ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import GHC.Exts (Proxy#)
import Unsafe.Coerce (unsafeCoerce)

-- | Convenient synonym to refer to the kind of a type variable:
-- @type KindOf (a :: k) = k@
#if __GLASGOW_HASKELL__ >= 810
type KindOf :: k -> Type
#endif
type KindOf (a :: k) = k

-- | Force GHC to unify the kinds of @a@ and @b@. Note that @SameKind a b@ is
-- different from @KindOf a ~ KindOf b@ in that the former makes the kinds
-- unify immediately, whereas the latter is a proposition that GHC considers
-- as possibly false.
#if __GLASGOW_HASKELL__ >= 810
type SameKind :: k -> k -> Constraint
#endif
type SameKind (a :: k) (b :: k) = (() :: Constraint)

----------------------------------------------------------------------
---- Sing & friends --------------------------------------------------
----------------------------------------------------------------------

-- | The singleton kind-indexed type family.
#if __GLASGOW_HASKELL__ >= 810
type Sing :: k -> Type
#endif
type family Sing :: k -> Type

{-
Note [The kind of Sing]
~~~~~~~~~~~~~~~~~~~~~~~
It is important to define Sing like this:

  type Sing :: k -> Type
  type family Sing

Or, equivalently,

  type family Sing :: k -> Type

There are other conceivable ways to define Sing, but they all suffer from
various drawbacks:

* type family Sing :: forall k. k -> Type

  Surprisingly, this is /not/ equivalent to `type family Sing :: k -> Type`.
  The difference lies in their arity, i.e., the number of arguments that must
  be supplied in order to apply Sing. The former declaration has arity 0, while
  the latter has arity 1 (this is more obvious if you write the declaration as
  GHCi would display it with -fprint-explicit-kinds enabled:
  `type family Sing @k :: k -> Type`).

  The former declaration having arity 0 is actually what makes it useless. If
  we were to adopt an arity-0 definition of `Sing`, then in order to write
  `type instance Sing = SFoo`, GHC would require that `SFoo` must have the kind
  `forall k. k -> Type`, and moreover, the kind /must/ be polymorphic in `k`.
  This is undesirable, because in practice, every single `Sing` instance in the
  wild must monomorphize `k` (e.g., `SBool` monomorphizes it to `Bool`), so an
  arity-0 `Sing` simply won't work. In contrast, the current arity-1 definition
  of `Sing` /does/ let you monomorphize `k` in type family instances.

* type family Sing (a :: k) = (r :: Type) | r -> a

  Again, this is not equivalent to `type family Sing :: k -> Type`. This
  version of `Sing` has arity 2, since one must supply both `k` and `a` in
  order to apply it. While an arity-2 `Sing` is not suffer from the same
  polymorphism issues as the arity-0 `Sing` in the previous bullet point, it
  does suffer from another issue in that it cannot be partially applied. This
  is because its `a` argument /must/ be supplied, whereas with the arity-1
  `Sing`, it is perfectly admissible to write `Sing` without an explicit `a`
  argument. (Its invisible `k` argument is filled in automatically behind the
  scenes.)

* type family Sing = (r :: k -> Type) | r -> k

  This is the same as `type family Sing :: k -> Type`, but with an injectivity
  annotation. Technically, this definition isn't /wrong/, but the injectivity
  annotation is actually unnecessary. Because the return kind of `Sing` is
  declared to be `k -> Type`, the `Sing` type constructor is automatically
  injective, so `Sing a1 ~ Sing a2` implies `a1 ~~ a2`.

  Another way of phrasing this, using the terminology of Dependent Haskell, is
  that the arrow in `Sing`'s return kind is /matchable/, which implies that
  `Sing` is an injective type constructor as a consequence.
-}

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI' or the 'Sing' pattern synonym.
#if __GLASGOW_HASKELL__ >= 900
type SingI :: forall {k}. k -> Constraint
#endif
class SingI a where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

-- | An explicitly bidirectional pattern synonym for implicit singletons.
--
-- As an __expression__: Constructs a singleton @Sing a@ given a
-- implicit singleton constraint @SingI a@.
--
-- As a __pattern__: Matches on an explicit @Sing a@ witness bringing
-- an implicit @SingI a@ constraint into scope.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Sing #-}
#endif
pattern Sing :: forall k (a :: k). () => SingI a => Sing a
pattern Sing <- (singInstance -> SingInstance)
  where Sing = sing

-- | The 'SingKind' class is a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
--
-- For a 'SingKind' instance to be well behaved, it should obey the following laws:
--
-- @
-- 'toSing' . 'fromSing' ≡ 'SomeSing'
-- (\\x -> 'withSomeSing' x 'fromSing') ≡ 'id'
-- @
--
-- The final law can also be expressed in terms of the 'FromSing' pattern
-- synonym:
--
-- @
-- (\\('FromSing' sing) -> 'FromSing' sing) ≡ 'id'
-- @
#if __GLASGOW_HASKELL__ >= 810
type SingKind :: Type -> Constraint
#endif
class SingKind k where
  -- | Get a base type from the promoted kind. For example,
  -- @Demote Bool@ will be the type @Bool@. Rarely, the type and kind do not
  -- match. For example, @Demote Nat@ is @Natural@.
  type Demote k = (r :: Type) | r -> k

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> Demote k

  -- | Convert an unrefined type to an existentially-quantified singleton type.
  toSing   :: Demote k -> SomeSing k

-- | An /existentially-quantified/ singleton. This type is useful when you want a
-- singleton type, but there is no way of knowing, at compile-time, what the type
-- index will be. To make use of this type, you will generally have to use a
-- pattern-match:
--
-- > foo :: Bool -> ...
-- > foo b = case toSing b of
-- >           SomeSing sb -> {- fancy dependently-typed code with sb -}
--
-- An example like the one above may be easier to write using 'withSomeSing'.
#if __GLASGOW_HASKELL__ >= 810
type SomeSing :: Type -> Type
#endif
data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

-- | An explicitly bidirectional pattern synonym for going between a
-- singleton and the corresponding demoted term.
--
-- As an __expression__: this takes a singleton to its demoted (base)
-- type.
--
-- >>> :t FromSing \@Bool
-- FromSing \@Bool :: Sing a -> Bool
-- >>> FromSing SFalse
-- False
--
-- As a __pattern__: It extracts a singleton from its demoted (base)
-- type.
--
-- @
-- singAnd :: 'Bool' -> 'Bool' -> 'SomeSing' 'Bool'
-- singAnd ('FromSing' singBool1) ('FromSing' singBool2) =
--   'SomeSing' (singBool1 %&& singBool2)
-- @
--
-- instead of writing it with 'withSomeSing':
--
-- @
-- singAnd bool1 bool2 =
--   'withSomeSing' bool1 $ \singBool1 ->
--     'withSomeSing' bool2 $ \singBool2 ->
--       'SomeSing' (singBool1 %&& singBool2)
-- @
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE FromSing #-}
#endif
pattern FromSing :: SingKind k => forall (a :: k). Sing a -> Demote k
pattern FromSing sng <- ((\demotedVal -> withSomeSing demotedVal SomeSing) -> SomeSing sng)
  where FromSing sng = fromSing sng

----------------------------------------------------------------------
---- WrappedSing -----------------------------------------------------
----------------------------------------------------------------------

-- | A newtype around 'Sing'.
--
-- Since 'Sing' is a type family, it cannot be used directly in type class
-- instances. As one example, one cannot write a catch-all
-- @instance 'SDecide' k => 'TestEquality' ('Sing' k)@. On the other hand,
-- 'WrappedSing' is a perfectly ordinary data type, which means that it is
-- quite possible to define an
-- @instance 'SDecide' k => 'TestEquality' ('WrappedSing' k)@.
#if __GLASGOW_HASKELL__ >= 810
type WrappedSing :: k -> Type
#endif
newtype WrappedSing :: forall k. k -> Type where
  WrapSing :: forall k (a :: k). { unwrapSing :: Sing a } -> WrappedSing a

-- | The singleton for 'WrappedSing's. Informally, this is the singleton type
-- for other singletons.
#if __GLASGOW_HASKELL__ >= 810
type SWrappedSing :: forall k (a :: k). WrappedSing a -> Type
#endif
newtype SWrappedSing :: forall k (a :: k). WrappedSing a -> Type where
  SWrapSing :: forall k (a :: k) (ws :: WrappedSing a).
               { sUnwrapSing :: Sing a } -> SWrappedSing ws
type instance Sing = SWrappedSing

#if __GLASGOW_HASKELL__ >= 810
type UnwrapSing :: forall k (a :: k). WrappedSing a -> Sing a
#endif
type family UnwrapSing (ws :: WrappedSing (a :: k)) :: Sing a where
  UnwrapSing ('WrapSing s) = s

instance SingKind (WrappedSing a) where
  type Demote (WrappedSing a) = WrappedSing a
  fromSing (SWrapSing s) = WrapSing s
  toSing (WrapSing s) = SomeSing $ SWrapSing s

instance forall a (s :: Sing a). SingI a => SingI ('WrapSing s) where
  sing = SWrapSing sing

----------------------------------------------------------------------
---- SingInstance ----------------------------------------------------
----------------------------------------------------------------------

-- | A 'SingInstance' wraps up a 'SingI' instance for explicit handling.
#if __GLASGOW_HASKELL__ >= 810
type SingInstance :: k -> Type
#endif
data SingInstance (a :: k) where
  SingInstance :: SingI a => SingInstance a

-- dirty implementation of explicit-to-implicit conversion
#if __GLASGOW_HASKELL__ >= 810
type DI :: k -> Type
#endif
newtype DI a = Don'tInstantiate (SingI a => SingInstance a)

-- | Get an implicit singleton (a 'SingI' instance) from an explicit one.
singInstance :: forall k (a :: k). Sing a -> SingInstance a
singInstance s = with_sing_i SingInstance
  where
    with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
    with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

----------------------------------------------------------------------
---- Defunctionalization ---------------------------------------------
----------------------------------------------------------------------

-- | Representation of the kind of a type-level function. The difference
-- between term-level arrows and this type-level arrow is that at the term
-- level applications can be unsaturated, whereas at the type level all
-- applications have to be fully saturated.
#if __GLASGOW_HASKELL__ >= 810
type TyFun :: Type -> Type -> Type
#endif
data TyFun :: Type -> Type -> Type

-- | Something of kind @a '~>' b@ is a defunctionalized type function that is
-- not necessarily generative or injective. Defunctionalized type functions
-- (also called \"defunctionalization symbols\") can be partially applied, even
-- if the original type function cannot be. For more information on how this
-- works, see the "Promotion and partial application" section of the
-- @<https://github.com/goldfirere/singletons/blob/master/README.md README>@.
--
-- Normal type-level arrows @(->)@ can be converted into defunctionalization
-- arrows @('~>')@ by the use of the 'TyCon' family of types. (Refer to the
-- Haddocks for 'TyCon1' to see an example of this in practice.) For this
-- reason, we do not make an effort to define defunctionalization symbols for
-- most type constructors of kind @a -> b@, as they can be used in
-- defunctionalized settings by simply applying @TyCon{N}@ with an appropriate
-- @N@.
--
-- This includes the @(->)@ type constructor itself, which is of kind
-- @'Type' -> 'Type' -> 'Type'@. One can turn it into something of kind
-- @'Type' '~>' 'Type' '~>' 'Type'@ by writing @'TyCon2' (->)@, or something of
-- kind @'Type' -> 'Type' '~>' 'Type'@ by writing @'TyCon1' ((->) t)@
-- (where @t :: 'Type'@).
#if __GLASGOW_HASKELL__ >= 810
type (~>) :: Type -> Type -> Type
#endif
type a ~> b = TyFun a b -> Type
infixr 0 ~>

-- | Type level function application
#if __GLASGOW_HASKELL__ >= 810
type Apply :: (k1 ~> k2) -> k1 -> k2
#endif
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2

-- | An infix synonym for `Apply`
#if __GLASGOW_HASKELL__ >= 810
type (@@) :: (k1 ~> k2) -> k1 -> k2
#endif
type a @@ b = Apply a b
infixl 9 @@

#if __GLASGOW_HASKELL__ >= 806
-- | Workhorse for the 'TyCon1', etc., types. This can be used directly
-- in place of any of the @TyConN@ types, but it will work only with
-- /monomorphic/ types. When GHC#14645 is fixed, this should fully supersede
-- the @TyConN@ types.
--
-- Note that this is only defined on GHC 8.6 or later. Prior to GHC 8.6,
-- 'TyCon1' /et al./ were defined as separate data types.
#if __GLASGOW_HASKELL__ >= 810
type TyCon :: (k1 -> k2) -> unmatchable_fun
#endif
data family TyCon :: (k1 -> k2) -> unmatchable_fun
-- That unmatchable_fun should really be a function of k1 and k2,
-- but GHC 8.4 doesn't support type family calls in the result kind
-- of a data family. It should. See GHC#14645.

-- The result kind of this is also a bit wrong; it should line
-- up with unmatchable_fun above. However, we can't do that
-- because GHC is too stupid to remember that f's kind can't
-- have more than one argument when kind-checking the RHS of
-- the second equation. Note that this infelicity is independent
-- of the problem in the kind of TyCon. There is no GHC ticket
-- here because dealing with inequality like this is hard, and
-- I (Richard) wasn't sure what concrete value the ticket would
-- have, given that we don't know how to begin fixing it.

-- | An \"internal\" definition used primary in the 'Apply' instance for
-- 'TyCon'.
--
-- Note that this only defined on GHC 8.6 or later.
#if __GLASGOW_HASKELL__ >= 810
type ApplyTyCon :: (k1 -> k2) -> (k1 ~> unmatchable_fun)
#endif
type family ApplyTyCon :: (k1 -> k2) -> (k1 ~> unmatchable_fun) where
#if __GLASGOW_HASKELL__ >= 808
  ApplyTyCon @k1 @(k2 -> k3) @unmatchable_fun = ApplyTyConAux2
  ApplyTyCon @k1 @k2         @k2              = ApplyTyConAux1
#else
  ApplyTyCon = (ApplyTyConAux2 :: (k1 -> k2 -> k3) -> (k1 ~> unmatchable_fun))
  ApplyTyCon = (ApplyTyConAux1 :: (k1 -> k2)       -> (k1 ~> k2))
#endif
-- Upon first glance, the definition of ApplyTyCon (as well as the
-- corresponding Apply instance for TyCon) seems a little indirect. One might
-- wonder why these aren't defined like so:
--
--   type family ApplyTyCon (f :: k1 -> k2) (x :: k1) :: k3 where
--     ApplyTyCon (f :: k1 -> k2 -> k3) x = TyCon (f x)
--     ApplyTyCon f x                     = f x
--
--   type instance Apply (TyCon f) x = ApplyTyCon f x
--
-- This also works, but it requires that ApplyTyCon always be applied to a
-- minimum of two arguments. In particular, this rules out a trick that we use
-- elsewhere in the library to write SingI instances for different TyCons,
-- which relies on partial applications of ApplyTyCon:
--
--   instance forall k1 k2 (f :: k1 -> k2).
--            ( forall a. SingI a => SingI (f a)
--            , (ApplyTyCon :: (k1 -> k2) -> (k1 ~> k2)) ~ ApplyTyConAux1
--            ) => SingI (TyCon1 f) where
type instance Apply (TyCon f) x = ApplyTyCon f @@ x

-- | An \"internal\" defunctionalization symbol used primarily in the
-- definition of 'ApplyTyCon', as well as the 'SingI' instances for 'TyCon1',
-- 'TyCon2', etc.
--
-- Note that this is only defined on GHC 8.6 or later.
#if __GLASGOW_HASKELL__ >= 810
type ApplyTyConAux1 :: (k1 -> k2) -> (k1 ~> k2)
#endif
data ApplyTyConAux1 :: (k1 -> k2) -> (k1 ~> k2)

-- | An \"internal\" defunctionalization symbol used primarily in the
-- definition of 'ApplyTyCon'.
--
-- Note that this is only defined on GHC 8.6 or later.
#if __GLASGOW_HASKELL__ >= 810
type ApplyTyConAux2 :: (k1 -> k2 -> k3) -> (k1 ~> unmatchable_fun)
#endif
data ApplyTyConAux2 :: (k1 -> k2 -> k3) -> (k1 ~> unmatchable_fun)

type instance Apply (ApplyTyConAux1 f) x = f x
type instance Apply (ApplyTyConAux2 f) x = TyCon (f x)

#if __GLASGOW_HASKELL__ >= 810
type TyCon1          :: (k1 -> k2) -> (k1 ~> k2)
type TyCon2          :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3)
type TyCon3          :: (k1 -> k2 -> k3 -> k4) -> (k1 ~> k2 ~> k3 ~> k4)
type TyCon4          :: (k1 -> k2 -> k3 -> k4 -> k5) -> (k1 ~> k2 ~> k3 ~> k4 ~> k5)
type TyCon5          :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6)
type TyCon6          :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7)
type TyCon7          :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7 ~> k8)
type TyCon8          :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7 ~> k8 ~> k9)
#endif

-- | Wrapper for converting the normal type-level arrow into a '~>'.
-- For example, given:
--
-- > data Nat = Zero | Succ Nat
-- > type family Map (a :: a ~> b) (a :: [a]) :: [b]
-- >   Map f '[] = '[]
-- >   Map f (x ': xs) = Apply f x ': Map f xs
--
-- We can write:
--
-- > Map (TyCon1 Succ) [Zero, Succ Zero]
type TyCon1 = (TyCon :: (k1 -> k2) -> (k1 ~> k2))

-- | Similar to 'TyCon1', but for two-parameter type constructors.
type TyCon2 = (TyCon :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3))
type TyCon3 = (TyCon :: (k1 -> k2 -> k3 -> k4) -> (k1 ~> k2 ~> k3 ~> k4))
type TyCon4 = (TyCon :: (k1 -> k2 -> k3 -> k4 -> k5) -> (k1 ~> k2 ~> k3 ~> k4 ~> k5))
type TyCon5 = (TyCon :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6))
type TyCon6 = (TyCon :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7))
type TyCon7 = (TyCon :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7 ~> k8))
type TyCon8 = (TyCon :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9)
                     -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7 ~> k8 ~> k9))
#else
-- | Wrapper for converting the normal type-level arrow into a '~>'.
-- For example, given:
--
-- > data Nat = Zero | Succ Nat
-- > type family Map (a :: a ~> b) (a :: [a]) :: [b]
-- >   Map f '[] = '[]
-- >   Map f (x ': xs) = Apply f x ': Map f xs
--
-- We can write:
--
-- > Map (TyCon1 Succ) [Zero, Succ Zero]
data TyCon1 :: (k1 -> k2) -> (k1 ~> k2)

-- | Similar to 'TyCon1', but for two-parameter type constructors.
data TyCon2 :: (k1 -> k2 -> k3) -> (k1 ~> k2 ~> k3)
data TyCon3 :: (k1 -> k2 -> k3 -> k4) -> (k1 ~> k2 ~> k3 ~> k4)
data TyCon4 :: (k1 -> k2 -> k3 -> k4 -> k5) -> (k1 ~> k2 ~> k3 ~> k4 ~> k5)
data TyCon5 :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6)
            -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6)
data TyCon6 :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7)
            -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7)
data TyCon7 :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8)
            -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7 ~> k8)
data TyCon8 :: (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9)
            -> (k1 ~> k2 ~> k3 ~> k4 ~> k5 ~> k6 ~> k7 ~> k8 ~> k9)

type instance Apply (TyCon1 f) x = f x
type instance Apply (TyCon2 f) x = TyCon1 (f x)
type instance Apply (TyCon3 f) x = TyCon2 (f x)
type instance Apply (TyCon4 f) x = TyCon3 (f x)
type instance Apply (TyCon5 f) x = TyCon4 (f x)
type instance Apply (TyCon6 f) x = TyCon5 (f x)
type instance Apply (TyCon7 f) x = TyCon6 (f x)
type instance Apply (TyCon8 f) x = TyCon7 (f x)
#endif

----------------------------------------------------------------------
---- Defunctionalized Sing instance and utilities --------------------
----------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 810
type SLambda :: (k1 ~> k2) -> Type
#endif
newtype SLambda (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }
type instance Sing = SLambda

-- | An infix synonym for `applySing`
(@@) :: forall k1 k2 (f :: k1 ~> k2) (t :: k1). Sing f -> Sing t -> Sing (f @@ t)
(@@) f = applySing f

-- | Note that this instance's 'toSing' implementation crucially relies on the fact
-- that the 'SingKind' instances for 'k1' and 'k2' both satisfy the 'SingKind' laws.
-- If they don't, 'toSing' might produce strange results!
instance (SingKind k1, SingKind k2) => SingKind (k1 ~> k2) where
  type Demote (k1 ~> k2) = Demote k1 -> Demote k2
  fromSing sFun x = withSomeSing x (fromSing . applySing sFun)
  toSing f = SomeSing slam
    where
      -- Here, we are essentially "manufacturing" a type-level version of the
      -- function f. As long as k1 and k2 obey the SingKind laws, this is a
      -- perfectly fine thing to do, since the computational content of Sing f
      -- will be isomorphic to that of the function f.
      slam :: forall (f :: k1 ~> k2). Sing f
      slam = singFun1 @f lam
        where
          -- Here's the tricky part. We need to demote the argument Sing, apply the
          -- term-level function f to it, and promote it back to a Sing. However,
          -- we don't have a way to convince the typechecker that for all argument
          -- types t, f @@ t should be the same thing as res, which motivates the
          -- use of unsafeCoerce.
          lam :: forall (t :: k1). Sing t -> Sing (f @@ t)
          lam x = withSomeSing (f (fromSing x)) (\(r :: Sing res) -> unsafeCoerce r)

#if __GLASGOW_HASKELL__ >= 810
type SingFunction1 :: (a1 ~> b) -> Type
type SingFunction2 :: (a1 ~> a2 ~> b) -> Type
type SingFunction3 :: (a1 ~> a2 ~> a3 ~> b) -> Type
type SingFunction4 :: (a1 ~> a2 ~> a3 ~> a4 ~> b) -> Type
type SingFunction5 :: (a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> b) -> Type
type SingFunction6 :: (a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> a6 ~> b) -> Type
type SingFunction7 :: (a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> a6 ~> a7 ~> b) -> Type
type SingFunction8 :: (a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> a6 ~> a7 ~> a8 ~> b) -> Type
#endif

type SingFunction1 (f :: a1 ~> b) =
  forall t. Sing t -> Sing (f @@ t)

-- | Use this function when passing a function on singletons as
-- a higher-order function. You will need visible type application
-- to get this to work. For example:
--
-- > falses = sMap (singFun1 @NotSym0 sNot)
-- >               (STrue `SCons` STrue `SCons` SNil)
--
-- There are a family of @singFun...@ functions, keyed by the number
-- of parameters of the function.
singFun1 :: forall f. SingFunction1 f -> Sing f
singFun1 f = SLambda f

type SingFunction2 (f :: a1 ~> a2 ~> b) =
  forall t1 t2. Sing t1 -> Sing t2 -> Sing (f @@ t1 @@ t2)
singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = SLambda (\x -> singFun1 (f x))

type SingFunction3 (f :: a1 ~> a2 ~> a3 ~> b) =
     forall t1 t2 t3.
     Sing t1 -> Sing t2 -> Sing t3
  -> Sing (f @@ t1 @@ t2 @@ t3)
singFun3 :: forall f. SingFunction3 f -> Sing f
singFun3 f = SLambda (\x -> singFun2 (f x))

type SingFunction4 (f :: a1 ~> a2 ~> a3 ~> a4 ~> b) =
     forall t1 t2 t3 t4.
     Sing t1 -> Sing t2 -> Sing t3 -> Sing t4
  -> Sing (f @@ t1 @@ t2 @@ t3 @@ t4)
singFun4 :: forall f. SingFunction4 f -> Sing f
singFun4 f = SLambda (\x -> singFun3 (f x))

type SingFunction5 (f :: a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> b) =
     forall t1 t2 t3 t4 t5.
     Sing t1 -> Sing t2 -> Sing t3 -> Sing t4 -> Sing t5
  -> Sing (f @@ t1 @@ t2 @@ t3 @@ t4 @@ t5)
singFun5 :: forall f. SingFunction5 f -> Sing f
singFun5 f = SLambda (\x -> singFun4 (f x))

type SingFunction6 (f :: a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> a6 ~> b) =
     forall t1 t2 t3 t4 t5 t6.
     Sing t1 -> Sing t2 -> Sing t3 -> Sing t4 -> Sing t5 -> Sing t6
  -> Sing (f @@ t1 @@ t2 @@ t3 @@ t4 @@ t5 @@ t6)
singFun6 :: forall f. SingFunction6 f -> Sing f
singFun6 f = SLambda (\x -> singFun5 (f x))

type SingFunction7 (f :: a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> a6 ~> a7 ~> b) =
     forall t1 t2 t3 t4 t5 t6 t7.
     Sing t1 -> Sing t2 -> Sing t3 -> Sing t4 -> Sing t5 -> Sing t6 -> Sing t7
  -> Sing (f @@ t1 @@ t2 @@ t3 @@ t4 @@ t5 @@ t6 @@ t7)
singFun7 :: forall f. SingFunction7 f -> Sing f
singFun7 f = SLambda (\x -> singFun6 (f x))

type SingFunction8 (f :: a1 ~> a2 ~> a3 ~> a4 ~> a5 ~> a6 ~> a7 ~> a8 ~> b) =
     forall t1 t2 t3 t4 t5 t6 t7 t8.
     Sing t1 -> Sing t2 -> Sing t3 -> Sing t4 -> Sing t5 -> Sing t6 -> Sing t7 -> Sing t8
  -> Sing (f @@ t1 @@ t2 @@ t3 @@ t4 @@ t5 @@ t6 @@ t7 @@ t8)
singFun8 :: forall f. SingFunction8 f -> Sing f
singFun8 f = SLambda (\x -> singFun7 (f x))

-- | This is the inverse of 'singFun1', and likewise for the other
-- @unSingFun...@ functions.
unSingFun1 :: forall f. Sing f -> SingFunction1 f
unSingFun1 sf = applySing sf

unSingFun2 :: forall f. Sing f -> SingFunction2 f
unSingFun2 sf x = unSingFun1 (sf @@ x)

unSingFun3 :: forall f. Sing f -> SingFunction3 f
unSingFun3 sf x = unSingFun2 (sf @@ x)

unSingFun4 :: forall f. Sing f -> SingFunction4 f
unSingFun4 sf x = unSingFun3 (sf @@ x)

unSingFun5 :: forall f. Sing f -> SingFunction5 f
unSingFun5 sf x = unSingFun4 (sf @@ x)

unSingFun6 :: forall f. Sing f -> SingFunction6 f
unSingFun6 sf x = unSingFun5 (sf @@ x)

unSingFun7 :: forall f. Sing f -> SingFunction7 f
unSingFun7 sf x = unSingFun6 (sf @@ x)

unSingFun8 :: forall f. Sing f -> SingFunction8 f
unSingFun8 sf x = unSingFun7 (sf @@ x)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE SLambda2 #-}
{-# COMPLETE SLambda3 #-}
{-# COMPLETE SLambda4 #-}
{-# COMPLETE SLambda5 #-}
{-# COMPLETE SLambda6 #-}
{-# COMPLETE SLambda7 #-}
{-# COMPLETE SLambda8 #-}
#endif

pattern SLambda2 :: forall f. SingFunction2 f -> Sing f
pattern SLambda2 {applySing2} <- (unSingFun2 -> applySing2)
  where SLambda2 lam2         = singFun2 lam2

pattern SLambda3 :: forall f. SingFunction3 f -> Sing f
pattern SLambda3 {applySing3} <- (unSingFun3 -> applySing3)
  where SLambda3 lam3         = singFun3 lam3

pattern SLambda4 :: forall f. SingFunction4 f -> Sing f
pattern SLambda4 {applySing4} <- (unSingFun4 -> applySing4)
  where SLambda4 lam4         = singFun4 lam4

pattern SLambda5 :: forall f. SingFunction5 f -> Sing f
pattern SLambda5 {applySing5} <- (unSingFun5 -> applySing5)
  where SLambda5 lam5         = singFun5 lam5

pattern SLambda6 :: forall f. SingFunction6 f -> Sing f
pattern SLambda6 {applySing6} <- (unSingFun6 -> applySing6)
  where SLambda6 lam6         = singFun6 lam6

pattern SLambda7 :: forall f. SingFunction7 f -> Sing f
pattern SLambda7 {applySing7} <- (unSingFun7 -> applySing7)
  where SLambda7 lam7         = singFun7 lam7

pattern SLambda8 :: forall f. SingFunction8 f -> Sing f
pattern SLambda8 {applySing8} <- (unSingFun8 -> applySing8)
  where SLambda8 lam8         = singFun8 lam8

----------------------------------------------------------------------
---- Convenience -----------------------------------------------------
----------------------------------------------------------------------

-- | Convenience function for creating a context with an implicit singleton
-- available.
withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

-- | Convert a normal datatype (like 'Bool') to a singleton for that datatype,
-- passing it into a continuation.
withSomeSing :: forall k r
              . SingKind k
             => Demote k                          -- ^ The original datatype
             -> (forall (a :: k). Sing a -> r)    -- ^ Function expecting a singleton
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'

-- | A convenience function useful when we need to name a singleton value
-- multiple times. Without this function, each use of 'sing' could potentially
-- refer to a different singleton, and one has to use type signatures (often
-- with @ScopedTypeVariables@) to ensure that they are the same.
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

-- | A convenience function that names a singleton satisfying a certain
-- property.  If the singleton does not satisfy the property, then the function
-- returns 'Nothing'. The property is expressed in terms of the underlying
-- representation of the singleton.
singThat :: forall k (a :: k). (SingKind k, SingI a)
         => (Demote k -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

-- | Allows creation of a singleton when a proxy is at hand.
singByProxy :: SingI a => proxy a -> Sing a
singByProxy _ = sing

-- | Allows creation of a singleton when a @proxy#@ is at hand.
singByProxy# :: SingI a => Proxy# a -> Sing a
singByProxy# _ = sing

-- | A convenience function that takes a type as input and demotes it to its
-- value-level counterpart as output. This uses 'SingKind' and 'SingI' behind
-- the scenes, so @'demote' = 'fromSing' 'sing'@.
--
-- This function is intended to be used with @TypeApplications@. For example:
--
-- >>> demote @True
-- True
--
-- >>> demote @(Nothing :: Maybe Ordering)
-- Nothing
demote ::
#if __GLASGOW_HASKELL__ >= 900
  forall {k} (a :: k). (SingKind k, SingI a) => Demote k
#else
  forall a. (SingKind (KindOf a), SingI a) => Demote (KindOf a)
#endif
demote = fromSing (sing @a)

----------------------------------------------------------------------
---- SingI TyCon{N} instances ----------------------------------------
----------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 806
instance forall k1 kr (f :: k1 -> kr).
         ( forall a. SingI a => SingI (f a)
         ,   (ApplyTyCon :: (k1 -> kr) -> (k1 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon1 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 kr (f :: k1 -> k2 -> kr).
         ( forall a b. (SingI a, SingI b) => SingI (f a b)
         ,   (ApplyTyCon :: (k2 -> kr) -> (k2 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon2 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 k3 kr (f :: k1 -> k2 -> k3 -> kr).
         ( forall a b c. (SingI a, SingI b, SingI c) => SingI (f a b c)
         ,   (ApplyTyCon :: (k3 -> kr) -> (k3 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon3 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 k3 k4 kr (f :: k1 -> k2 -> k3 -> k4 -> kr).
         ( forall a b c d. (SingI a, SingI b, SingI c, SingI d) => SingI (f a b c d)
         ,   (ApplyTyCon :: (k4 -> kr) -> (k4 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon4 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 k3 k4 k5 kr
                (f :: k1 -> k2 -> k3 -> k4 -> k5 -> kr).
         ( forall a b c d e.
              (SingI a, SingI b, SingI c, SingI d, SingI e)
           => SingI (f a b c d e)
         ,   (ApplyTyCon :: (k5 -> kr) -> (k5 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon5 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 k3 k4 k5 k6 kr
                (f :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> kr).
         ( forall a b c d e f'.
              (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f')
           => SingI (f a b c d e f')
         ,   (ApplyTyCon :: (k6 -> kr) -> (k6 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon6 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 k3 k4 k5 k6 k7 kr
                (f :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> kr).
         ( forall a b c d e f' g.
              (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f', SingI g)
           => SingI (f a b c d e f' g)
         ,   (ApplyTyCon :: (k7 -> kr) -> (k7 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon7 f) where
  sing = singFun1 (`withSingI` sing)
instance forall k1 k2 k3 k4 k5 k6 k7 k8 kr
                (f :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> kr).
         ( forall a b c d e f' g h.
              (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f', SingI g, SingI h)
           => SingI (f a b c d e f' g h)
         ,   (ApplyTyCon :: (k8 -> kr) -> (k8 ~> kr))
           ~ ApplyTyConAux1
         ) => SingI (TyCon8 f) where
  sing = singFun1 (`withSingI` sing)
#endif

----------------------------------------------------------------------
---- Defunctionalization symbols -------------------------------------
----------------------------------------------------------------------

-- $(genDefunSymbols [''Demote, ''SameKind, ''KindOf, ''(~>), ''Apply, ''(@@)])
-- WrapSing, UnwrapSing, and SingFunction1 et al. are not defunctionalizable
-- at the moment due to GHC#9269

#if __GLASGOW_HASKELL__ >= 810
type DemoteSym0 :: Type ~> Type
type DemoteSym1 :: Type -> Type
#endif

data DemoteSym0 :: Type ~> Type
type DemoteSym1 x = Demote x

type instance Apply DemoteSym0 x = Demote x

-----

#if __GLASGOW_HASKELL__ >= 810
type SameKindSym0 :: forall k. k ~> k ~> Constraint
type SameKindSym1 :: forall k. k -> k ~> Constraint
type SameKindSym2 :: forall k. k -> k -> Constraint
#endif

data SameKindSym0 :: forall k. k ~> k ~> Constraint
data SameKindSym1 :: forall k. k -> k ~> Constraint
type SameKindSym2 (x :: k) (y :: k) = SameKind x y

type instance Apply SameKindSym0 x = SameKindSym1 x
type instance Apply (SameKindSym1 x) y = SameKind x y

-----

#if __GLASGOW_HASKELL__ >= 810
type KindOfSym0 :: forall k. k ~> Type
type KindOfSym1 :: forall k. k -> Type
#endif

data KindOfSym0 :: forall k. k ~> Type
type KindOfSym1 (x :: k) = KindOf x

type instance Apply KindOfSym0 x = KindOf x

-----

infixr 0 ~>@#@$, ~>@#@$$, ~>@#@$$$

#if __GLASGOW_HASKELL__ >= 810
type (~>@#@$)  :: Type ~> Type ~> Type
type (~>@#@$$) :: Type -> Type ~> Type
type (~>@#@$$$) :: Type -> Type -> Type
#endif

data (~>@#@$)  :: Type ~> Type ~> Type
data (~>@#@$$) :: Type -> Type ~> Type
type x ~>@#@$$$ y = x ~> y

type instance Apply (~>@#@$) x = (~>@#@$$) x
type instance Apply ((~>@#@$$) x) y = x ~> y

-----

#if __GLASGOW_HASKELL__ >= 810
type ApplySym0 :: forall a b. (a ~> b) ~> a ~> b
type ApplySym1 :: forall a b. (a ~> b) -> a ~> b
type ApplySym2 :: forall a b. (a ~> b) -> a -> b
#endif

data ApplySym0 :: forall a b. (a ~> b) ~> a ~> b
data ApplySym1 :: forall a b. (a ~> b) -> a ~> b
type ApplySym2 (f :: a ~> b) (x :: a) = Apply f x

type instance Apply ApplySym0 f = ApplySym1 f
type instance Apply (ApplySym1 f) x = Apply f x

-----

infixl 9 @@@#@$, @@@#@$$, @@@#@$$$

#if __GLASGOW_HASKELL__ >= 810
type (@@@#@$)  :: forall a b. (a ~> b) ~> a ~> b
type (@@@#@$$) :: forall a b. (a ~> b) -> a ~> b
type (@@@#@$$$) :: forall a b. (a ~> b) -> a -> b
#endif

data (@@@#@$)  :: forall a b. (a ~> b) ~> a ~> b
data (@@@#@$$) :: forall a b. (a ~> b) -> a ~> b
type (f :: a ~> b) @@@#@$$$ (x :: a) = f @@ x

type instance Apply (@@@#@$) f = (@@@#@$$) f
type instance Apply ((@@@#@$$) f) x = f @@ x

{- $SingletonsOfSingletons

Aside from being a data type to hang instances off of, 'WrappedSing' has
another purpose as a general-purpose mechanism for allowing one to write
code that uses singletons of other singletons. For instance, suppose you
had the following data type:

@
data T :: Type -> Type where
  MkT :: forall a (x :: a). 'Sing' x -> F a -> T a
@

A naïve attempt at defining a singleton for @T@ would look something like
this:

@
data ST :: forall a. T a -> Type where
  SMkT :: forall a (x :: a) (sx :: 'Sing' x) (f :: F a).
          'Sing' sx -> 'Sing' f -> ST (MkT sx f)
@

But there is a problem here: what exactly /is/ @'Sing' sx@? If @x@ were 'True',
for instance, then @sx@ would be 'STrue', but it's not clear what
@'Sing' 'STrue'@ should be. One could define @SSBool@ to be the singleton of
'SBool's, but in order to be thorough, one would have to generate a singleton
for /every/ singleton type out there. Plus, it's not clear when to stop. Should
we also generate @SSSBool@, @SSSSBool@, etc.?

Instead, 'WrappedSing' and its singleton 'SWrappedSing' provide a way to talk
about singletons of other arbitrary singletons without the need to generate a
bazillion instances. For reference, here is the definition of 'SWrappedSing':

@
newtype 'SWrappedSing' :: forall k (a :: k). 'WrappedSing' a -> Type where
  'SWrapSing' :: forall k (a :: k) (ws :: 'WrappedSing' a).
                 { 'sUnwrapSing' :: 'Sing' a } -> 'SWrappedSing' ws
type instance 'Sing' \@('WrappedSing' a) = 'SWrappedSing'
@

'SWrappedSing' is a bit of an unusual singleton in that its field is a
singleton for @'Sing' \@k@, not @'WrappedSing' \@k@. But that's exactly the
point—a singleton of a singleton contains as much type information as the
underlying singleton itself, so we can get away with just @'Sing' \@k@.

As an example of this in action, here is how you would define the singleton
for the earlier @T@ type:

@
data ST :: forall a. T a -> Type where
  SMkT :: forall a (x :: a) (sx :: 'Sing' x) (f :: F a).
          'Sing' ('WrapSing' sx) -> 'Sing' f -> ST (MkT sx f)
@

With this technique, we won't need anything like @SSBool@ in order to
instantiate @x@ with 'True'. Instead, the field of type
@'Sing' ('WrapSing' sx)@ will simply be a newtype around 'SBool'. In general,
you'll need /n/ layers of 'WrapSing' if you wish to single a singleton /n/
times.

Note that this is not the only possible way to define a singleton for @T@.
An alternative approach that does not make use of singletons-of-singletons is
discussed at some length
<https://github.com/goldfirere/singletons/issues/366#issuecomment-489469086 here>.
Due to the technical limitations of this approach, however, we do not use it
in @singletons@ at the moment, instead favoring the
slightly-clunkier-but-more-reliable 'WrappedSing' approach.
-}

{- $SLambdaPatternSynonyms

@SLambda{2...8}@ are explicitly bidirectional pattern synonyms for
defunctionalized singletons (@'Sing' (f :: k '~>' k' '~>' k'')@).

As __constructors__: Same as @singFun{2..8}@. For example, one can turn a
binary function on singletons @sTake :: 'SingFunction2' TakeSym0@ into a
defunctionalized singleton @'Sing' (TakeSym :: Nat '~>' [a] '~>' [a])@:

@
>>> import Data.List.Singletons
>>> :set -XTypeApplications
>>>
>>> :t 'SLambda2'
'SLambda2' :: 'SingFunction2' f -> 'Sing' f
>>> :t 'SLambda2' \@TakeSym0
'SLambda2' :: 'SingFunction2' TakeSym0 -> 'Sing' TakeSym0
>>> :t 'SLambda2' \@TakeSym0 sTake
'SLambda2' :: 'Sing' TakeSym0
@

This is useful for functions on singletons that expect a defunctionalized
singleton as an argument, such as @sZipWith :: 'SingFunction3' ZipWithSym0@:

@
sZipWith :: Sing (f :: a '~>' b '~>' c) -> Sing (xs :: [a]) -> Sing (ys :: [b]) -> Sing (ZipWith f xs ys :: [c])
sZipWith ('SLambda2' \@TakeSym0 sTake) :: Sing (xs :: [Nat]) -> Sing (ys :: [[a]]) -> Sing (ZipWith TakeSym0 xs ys :: [[a]])
@

As __patterns__: Same as @unSingFun{2..8}@. Gets a binary term-level
Haskell function on singletons
@'Sing' (x :: k) -> 'Sing' (y :: k') -> 'Sing' (f \@\@ x \@\@ y)@
from a defunctionalised @'Sing' f@. Alternatively, as a record field accessor:

@
applySing2 :: 'Sing' (f :: k '~>' k' '~>' k'') -> 'SingFunction2' f
@
-}
