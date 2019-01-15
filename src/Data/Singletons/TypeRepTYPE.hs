{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleInstances,
             GADTs, UndecidableInstances, ScopedTypeVariables,
             MagicHash, TypeOperators, PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeRepTYPE
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines singleton instances making 'TypeRep' the singleton for
-- the kind @'TYPE' rep@ (for some 'RuntimeRep' @rep@), an instantiation of
-- which is the famous kind 'Type'. The definitions don't fully line up with
-- what is expected within the singletons library, so expect unusual results!
--
----------------------------------------------------------------------------

module Data.Singletons.TypeRepTYPE (
  Sing(STypeRep),
  -- | Here is the definition of the singleton for @'TYPE' rep@:
  --
  -- > newtype instance Sing :: forall (rep :: RuntimeRep). TYPE rep -> Type where
  -- >   STypeRep :: forall (rep :: RuntimeRep) (a :: TYPE rep). TypeRep a -> Sing a
  --
  -- Instances for 'SingI', 'SingKind', 'SEq', 'SDecide', and
  -- 'TestCoercion' are also supplied.

  SomeTypeRepTYPE(..)
  ) where

import Data.Kind (Type)
import Data.Singletons.Prelude.Instances
import Data.Singletons.Internal
import Data.Singletons.Prelude.Eq
import Data.Singletons.Decide
import GHC.Exts (RuntimeRep, TYPE)
import Type.Reflection
import Type.Reflection.Unsafe
import Unsafe.Coerce

-- | A choice of singleton for the kind @'TYPE' rep@ (for some 'RuntimeRep'
-- @rep@), an instantiation of which is the famous kind 'Type'.
--
-- Conceivably, one could generalize this instance to `Sing :: k -> Type` for
-- /any/ kind @k@, and remove all other 'Sing' instances. We don't adopt this
-- design, however, since it is far more convenient in practice to work with
-- explicit singleton values than 'TypeRep's (for instance, 'TypeRep's are
-- more difficult to pattern match on, and require extra runtime checks).
--
-- We cannot produce explicit singleton values for everything in @'TYPE' rep@,
-- however, since it is an open kind, so we reach for 'TypeRep' in this one
-- particular case.
newtype instance Sing :: forall (rep :: RuntimeRep). TYPE rep -> Type where
  STypeRep :: forall (rep :: RuntimeRep) (a :: TYPE rep). TypeRep a -> Sing a
    deriving (Eq, Ord, Show)

-- | A variant of 'SomeTypeRep' whose underlying 'TypeRep' is restricted to
-- kind @'TYPE' rep@ (for some 'RuntimeRep' @rep@).
data SomeTypeRepTYPE :: RuntimeRep -> Type where
  SomeTypeRepTYPE :: forall (rep :: RuntimeRep) (a :: TYPE rep). !(TypeRep a) -> SomeTypeRepTYPE rep

instance Eq (SomeTypeRepTYPE rep) where
  SomeTypeRepTYPE a == SomeTypeRepTYPE b =
    case eqTypeRep a b of
      Just HRefl -> True
      Nothing    -> False

instance Ord (SomeTypeRepTYPE rep) where
  SomeTypeRepTYPE a `compare` SomeTypeRepTYPE b =
    typeRepFingerprint a `compare` typeRepFingerprint b

instance Show (SomeTypeRepTYPE rep) where
  showsPrec p (SomeTypeRepTYPE ty) = showsPrec p ty

instance Typeable a => SingI (a :: TYPE rep) where
  sing = STypeRep typeRep
instance SingKind (TYPE rep) where
  type Demote (TYPE rep) = SomeTypeRepTYPE rep
  fromSing (STypeRep tr) = SomeTypeRepTYPE tr
  toSing (SomeTypeRepTYPE tr) = SomeSing $ STypeRep tr

instance PEq (TYPE rep)
instance SEq (TYPE rep) where
  STypeRep tra %== STypeRep trb =
    case eqTypeRep tra trb of
      Just HRefl -> STrue
      Nothing    -> unsafeCoerce SFalse
                    -- the Data.Typeable interface isn't strong enough
                    -- to enable us to define this without unsafeCoerce

instance SDecide (TYPE rep) where
  STypeRep tra %~ STypeRep trb =
    case eqTypeRep tra trb of
      Just HRefl -> Proved Refl
      Nothing    -> Disproved (\Refl -> error "Type.Reflection.eqTypeRep failed")
