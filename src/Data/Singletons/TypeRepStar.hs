{-# LANGUAGE RankNTypes, TypeFamilies, KindSignatures, FlexibleInstances,
             GADTs, UndecidableInstances, ScopedTypeVariables, DataKinds,
             MagicHash, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeRepStar
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines singleton instances making 'TypeRep' the singleton for
-- the kind @*@. The definitions don't fully line up with what is expected
-- within the singletons library, so expect unusual results!
--
----------------------------------------------------------------------------

module Data.Singletons.TypeRepStar (
  Sing(STypeRep),
  -- | Here is the definition of the singleton for @*@:
  --
  -- > newtype instance Sing (a :: *) where
  -- >   STypeRep :: TypeRep a -> Sing a
  --
  -- Instances for 'SingI', 'SingKind', 'SEq', 'SDecide', and 'TestCoercion' are
  -- also supplied.

  SomeTypeRepStar(..)
  ) where

import Data.Singletons.Prelude.Instances
import Data.Singletons.Internal
import Data.Singletons.Prelude.Eq
import Data.Singletons.Decide
import Data.Singletons.ShowSing
import Type.Reflection
import Type.Reflection.Unsafe
import Unsafe.Coerce

import Data.Kind
import Data.Type.Coercion
import qualified Data.Type.Equality as DTE
import Data.Type.Equality ((:~:)(..))

newtype instance Sing (a :: *) where
  STypeRep :: TypeRep a -> Sing a
    deriving (Eq, Ord, Show)

-- | A variant of 'SomeTypeRep' whose underlying 'TypeRep' is restricted to
-- kind @*@.
data SomeTypeRepStar where
  SomeTypeRepStar :: forall (a :: *). !(TypeRep a) -> SomeTypeRepStar

instance Eq SomeTypeRepStar where
  SomeTypeRepStar a == SomeTypeRepStar b =
    case eqTypeRep a b of
      Just HRefl -> True
      Nothing    -> False

instance Ord SomeTypeRepStar where
  SomeTypeRepStar a `compare` SomeTypeRepStar b =
    typeRepFingerprint a `compare` typeRepFingerprint b

instance Show SomeTypeRepStar where
  showsPrec p (SomeTypeRepStar ty) = showsPrec p ty

instance Typeable a => SingI (a :: *) where
  sing = STypeRep typeRep
instance SingKind Type where
  type Demote Type = SomeTypeRepStar
  fromSing (STypeRep tr) = SomeTypeRepStar tr
  toSing (SomeTypeRepStar tr) = SomeSing $ STypeRep tr

instance PEq Type where
  type (a :: *) == (b :: *) = a DTE.== b

instance SEq Type where
  STypeRep tra %== STypeRep trb =
    case eqTypeRep tra trb of
      Just HRefl -> STrue
      Nothing    -> unsafeCoerce SFalse
                    -- the Data.Typeable interface isn't strong enough
                    -- to enable us to define this without unsafeCoerce

instance SDecide Type where
  STypeRep tra %~ STypeRep trb =
    case eqTypeRep tra trb of
      Just HRefl -> Proved Refl
      Nothing    -> Disproved (\Refl -> error "Type.Reflection.eqTypeRep failed")

instance ShowSing Type where
  showsSingPrec = showsPrec

-- TestEquality instance already defined, but we need this one:
instance TestCoercion Sing where
  testCoercion (STypeRep tra) (STypeRep trb) =
    case eqTypeRep tra trb of
      Just HRefl -> Just Coercion
      Nothing    -> Nothing
