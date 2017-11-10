{-# LANGUAGE MagicHash, RankNTypes, PolyKinds, GADTs, DataKinds,
             FlexibleContexts, FlexibleInstances,
             TypeFamilies, TypeOperators, TypeFamilyDependencies,
             UndecidableInstances, TypeInType, ConstraintKinds,
             ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes,
             CPP #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Internal
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the basic definitions to use singletons. This module
-- exists since we need to define instances for 'SomeSing' in
-- "Data.Singletons", as defining them elsewhere would almost inevitably lead
-- to import cycles.
--
----------------------------------------------------------------------------

module Data.Singletons.Internal (
    module Data.Singletons.Internal
  , Proxy(..)
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
-- see 'withSingI' or pattern match using the 'SingI' pattern synonym.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

#if __GLASGOW_HASKELL__ >= 710
-- | Pattern matching on an explicit @Sing a@ to get an implicit
-- @SingI a@ constraint.
--
-- /Since: 2.4/
#if __GLASGOW_HASKELL__ >= 800
pattern SingI :: forall (a::k). () => SingI a => Sing a
#endif
pattern SingI <- (singInstance -> SingInstance)
  where SingI = sing
#endif

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
class SingKind k where
  -- | Get a base type from the promoted kind. For example,
  -- @Demote Bool@ will be the type @Bool@. Rarely, the type and kind do not
  -- match. For example, @Demote Nat@ is @Natural@.
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
singThat :: forall (a :: k). (SingKind k, SingI a)
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
demote :: forall a. (SingKind (KindOf a), SingI a) => Demote (KindOf a)
demote = fromSing (sing @(KindOf a) @a)
