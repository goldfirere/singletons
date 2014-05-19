{-# LANGUAGE RankNTypes, TypeFamilies, KindSignatures, FlexibleInstances,
             GADTs, UndecidableInstances, ScopedTypeVariables, DataKinds,
             MagicHash, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeRepStar
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines singleton instances making 'Typeable' the singleton for
-- the kind @*@. The definitions don't fully line up with what is expected
-- within the singletons library, so expect unusual results!
--
----------------------------------------------------------------------------

module Data.Singletons.TypeRepStar (
  Sing(STypeRep)
  -- | Here is the definition of the singleton for @*@:
  --
  -- > data instance Sing (a :: *) where
  -- >   STypeRep :: Typeable a => Sing a
  --
  -- Instances for 'SingI', 'SingKind', 'SEq', 'SDecide', and 'TestCoercion' are
  -- also supplied.
  ) where

import Data.Singletons.Prelude.Instances
import Data.Singletons
import Data.Singletons.Prelude.Eq
import Data.Typeable
import Unsafe.Coerce
import Data.Singletons.Decide

import GHC.Exts ( Proxy# )
import Data.Type.Coercion
import Data.Type.Equality

data instance Sing (a :: *) where
  STypeRep :: Typeable a => Sing a

instance Typeable a => SingI (a :: *) where
  sing = STypeRep
instance SingKind ('KProxy :: KProxy *) where
  type DemoteRep ('KProxy :: KProxy *) = TypeRep
  fromSing (STypeRep :: Sing a) = typeOf (undefined :: a)
  toSing = dirty_mk_STypeRep

instance PEq ('KProxy :: KProxy *) where
  type (a :: *) :== (b :: *) = a == b

instance SEq ('KProxy :: KProxy *) where
  (STypeRep :: Sing a) %:== (STypeRep :: Sing b) =
    case (eqT :: Maybe (a :~: b)) of
      Just Refl -> STrue
      Nothing   -> unsafeCoerce SFalse
                    -- the Data.Typeable interface isn't strong enough
                    -- to enable us to define this without unsafeCoerce

instance SDecide ('KProxy :: KProxy *) where
  (STypeRep :: Sing a) %~ (STypeRep :: Sing b) =
    case (eqT :: Maybe (a :~: b)) of
      Just Refl -> Proved Refl
      Nothing   -> Disproved (\Refl -> error "Data.Typeable.eqT failed")

-- TestEquality instance already defined, but we need this one:
instance TestCoercion Sing where
  testCoercion (STypeRep :: Sing a) (STypeRep :: Sing b) =
    case (eqT :: Maybe (a :~: b)) of
      Just Refl -> Just Coercion
      Nothing   -> Nothing

-- everything below here is private and dirty. Don't look!

newtype DI = Don'tInstantiate (Typeable a => Sing a)
dirty_mk_STypeRep :: TypeRep -> SomeSing ('KProxy :: KProxy *)
dirty_mk_STypeRep rep =
  let justLikeTypeable :: Proxy# a -> TypeRep
      justLikeTypeable _ = rep
  in
  unsafeCoerce (Don'tInstantiate STypeRep) justLikeTypeable
