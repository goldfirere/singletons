{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the basic definitions to use singletons. For routine
-- use, consider importing 'Data.Singletons.Prelude', which exports constructors
-- for singletons based on types in the @Prelude@.
--
-- You may also want to read
-- the original papers presenting this library, available at
-- <http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf>
-- and <http://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf>.
--
----------------------------------------------------------------------------

module Data.Singletons (
  -- * Main singleton definitions

  Sing(SLambda, applySing), (@@),
  -- | See also 'Data.Singletons.Prelude.Sing' for exported constructors

  SingI(..), SingKind(..),

  -- * Working with singletons
  KindOf, SameKind,
  SingInstance(..), SomeSing(..),
  singInstance, withSingI, withSomeSing, singByProxy,

  singByProxy#,
  withSing, singThat,

  -- ** Defunctionalization
  TyFun, type (~>),
  TyCon1, TyCon2, TyCon3, TyCon4, TyCon5, TyCon6, TyCon7, TyCon8,
  Apply, type (@@),

  -- ** Defunctionalized singletons
  -- | When calling a higher-order singleton function, you need to use a
  -- @singFun...@ function to wrap it. See 'singFun1'.
  singFun1, singFun2, singFun3, singFun4, singFun5, singFun6, singFun7,
  singFun8,
  unSingFun1, unSingFun2, unSingFun3, unSingFun4, unSingFun5,
  unSingFun6, unSingFun7, unSingFun8,

  -- | These type synonyms are exported only to improve error messages; users
  -- should not have to mention them.
  SingFunction1, SingFunction2, SingFunction3, SingFunction4, SingFunction5,
  SingFunction6, SingFunction7, SingFunction8,

  -- * Auxiliary functions
  Proxy(..)
  ) where

import Data.Kind
import Unsafe.Coerce
import Data.Proxy ( Proxy(..) )
import GHC.Exts ( Proxy#, Constraint )

-- | Convenient synonym to refer to the kind of a type variable:
-- @type KindOf (a :: k) = k@
type KindOf (a :: k) = k

-- | Force GHC to unify the kinds of @a@ and @b@. Note that @SameKind a b@ is
-- different from @KindOf a ~ KindOf b@ in that the former makes the kinds
-- unify immediately, whereas the latter is a proposition that GHC considers
-- as possibly false.
type SameKind (a :: k) (b :: k) = (() :: Constraint)

----------------------------------------------------------------------
---- Sing & friends --------------------------------------------------
----------------------------------------------------------------------

-- | The singleton kind-indexed data family.
data family Sing (a :: k)

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI'.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

-- | The 'SingKind' class is a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
class SingKind k where
  -- | Get a base type from the promoted kind. For example,
  -- @Demote Bool@ will be the type @Bool@. Rarely, the type and kind do not
  -- match. For example, @Demote Nat@ is @Integer@.
  type Demote k = (r :: *) | r -> k

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
data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

----------------------------------------------------------------------
---- SingInstance ----------------------------------------------------
----------------------------------------------------------------------

-- | A 'SingInstance' wraps up a 'SingI' instance for explicit handling.
data SingInstance (a :: k) where
  SingInstance :: SingI a => SingInstance a

-- dirty implementation of explicit-to-implicit conversion
newtype DI a = Don'tInstantiate (SingI a => SingInstance a)

-- | Get an implicit singleton (a 'SingI' instance) from an explicit one.
singInstance :: forall (a :: k). Sing a -> SingInstance a
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
data TyFun :: * -> * -> *

-- | Something of kind `a ~> b` is a defunctionalized type function that is
-- not necessarily generative or injective.
type a ~> b = TyFun a b -> *
infixr 0 ~>

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

-- | Type level function application
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type instance Apply (TyCon1 f) x = f x
type instance Apply (TyCon2 f) x = TyCon1 (f x)
type instance Apply (TyCon3 f) x = TyCon2 (f x)
type instance Apply (TyCon4 f) x = TyCon3 (f x)
type instance Apply (TyCon5 f) x = TyCon4 (f x)
type instance Apply (TyCon6 f) x = TyCon5 (f x)
type instance Apply (TyCon7 f) x = TyCon6 (f x)
type instance Apply (TyCon8 f) x = TyCon7 (f x)

-- | An infix synonym for `Apply`
type a @@ b = Apply a b
infixl 9 @@

----------------------------------------------------------------------
---- Defunctionalized Sing instance and utilities --------------------
----------------------------------------------------------------------

newtype instance Sing (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }

-- | An infix synonym for `applySing`
(@@) :: forall (f :: k1 ~> k2) (t :: k1). Sing f -> Sing t -> Sing (f @@ t)
(@@) = applySing

instance (SingKind k1, SingKind k2) => SingKind (k1 ~> k2) where
  type Demote (k1 ~> k2) = Demote k1 -> Demote k2
  fromSing sFun x = withSomeSing x (fromSing . applySing sFun)
  toSing _ = error "Cannot create existentially-quantified singleton functions."

type SingFunction1 f = forall t. Sing t -> Sing (f @@ t)

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

type SingFunction2 f = forall t. Sing t -> SingFunction1 (f @@ t)
singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = SLambda (\x -> singFun1 (f x))

type SingFunction3 f = forall t. Sing t -> SingFunction2 (f @@ t)
singFun3 :: forall f. SingFunction3 f -> Sing f
singFun3 f = SLambda (\x -> singFun2 (f x))

type SingFunction4 f = forall t. Sing t -> SingFunction3 (f @@ t)
singFun4 :: forall f. SingFunction4 f -> Sing f
singFun4 f = SLambda (\x -> singFun3 (f x))

type SingFunction5 f = forall t. Sing t -> SingFunction4 (f @@ t)
singFun5 :: forall f. SingFunction5 f -> Sing f
singFun5 f = SLambda (\x -> singFun4 (f x))

type SingFunction6 f = forall t. Sing t -> SingFunction5 (f @@ t)
singFun6 :: forall f. SingFunction6 f -> Sing f
singFun6 f = SLambda (\x -> singFun5 (f x))

type SingFunction7 f = forall t. Sing t -> SingFunction6 (f @@ t)
singFun7 :: forall f. SingFunction7 f -> Sing f
singFun7 f = SLambda (\x -> singFun6 (f x))

type SingFunction8 f = forall t. Sing t -> SingFunction7 (f @@ t)
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

----------------------------------------------------------------------
---- SomeSing instances ----------------------------------------------
----------------------------------------------------------------------

instance SEq k => Eq (SomeSing k) where
  SomeSing a == SomeSing b = fromSing (a %:== b)
  SomeSing a /= SomeSing b = fromSing (a %:/= b)

instance SOrd k => Ord (SomeSing k) where
  SomeSing a `compare` SomeSing b = fromSing (a `sCompare` b)
  SomeSing a <         SomeSing b = fromSing (a %:<  b)
  SomeSing a <=        SomeSing b = fromSing (a %:<= b)
  SomeSing a >         SomeSing b = fromSing (a %:>  b)
  SomeSing a >=        SomeSing b = fromSing (a %:>= b)

instance SBounded k => Bounded (SomeSing k) where
  minBound = SomeSing sMinBound
  maxBound = SomeSing sMaxBound

instance (SEnum k, SingKind k) => Enum (SomeSing k) where
  succ (SomeSing a) = SomeSing (sSucc a)
  pred (SomeSing a) = SomeSing (sPred a)
  toEnum n = withSomeSing (fromIntegral n) (SomeSing . sToEnum)
  fromEnum (SomeSing a) = fromIntegral (fromSing (sFromEnum a))
  enumFromTo (SomeSing from) (SomeSing to) =
    map toSing (fromSing (sEnumFromTo from to))
  enumFromThenTo (SomeSing from) (SomeSing then_) (SomeSing to) =
    map toSing (fromSing (sEnumFromThenTo from then_ to))

instance SNum k => Num (SomeSing k) where
  SomeSing a + SomeSing b = SomeSing (a %:+ b)
  SomeSing a - SomeSing b = SomeSing (a %:- b)
  SomeSing a * SomeSing b = SomeSing (a %:* b)
  negate (SomeSing a) = SomeSing (sNegate a)
  abs    (SomeSing a) = SomeSing (sAbs a)
  signum (SomeSing a) = SomeSing (sSignum a)
  fromInteger n = withSomeSing (fromIntegral n) (SomeSing . sFromInteger)
