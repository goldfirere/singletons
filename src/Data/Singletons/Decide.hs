{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeOperators,
             TypeFamilies, FlexibleContexts, UndecidableInstances,
             GADTs, TypeApplications, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Decide
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the class 'SDecide', allowing for decidable equality over singletons.
--
----------------------------------------------------------------------------

module Data.Singletons.Decide (
  -- * The SDecide class
  SDecide(..),

  -- * Supporting definitions
  (:~:)(..), Void, Refuted, Decision(..),
  decideEquality, decideCoercion
  ) where

import Data.Kind
import Data.Singletons.Internal
import Data.Type.Coercion
import Data.Type.Equality
import Data.Void

----------------------------------------------------------------------
---- SDecide ---------------------------------------------------------
----------------------------------------------------------------------

-- | Because we can never create a value of type 'Void', a function that type-checks
-- at @a -> Void@ shows that objects of type @a@ can never exist. Thus, we say that
-- @a@ is 'Refuted'
type Refuted :: Type -> Type
type Refuted a = (a -> Void)

-- | A 'Decision' about a type @a@ is either a proof of existence or a proof that @a@
-- cannot exist.
type Decision :: Type -> Type
data Decision a = Proved a               -- ^ Witness for @a@
                | Disproved (Refuted a)  -- ^ Proof that no @a@ exists

-- | Members of the 'SDecide' "kind" class support decidable equality. Instances
-- of this class are generated alongside singleton definitions for datatypes that
-- derive an 'Eq' instance.
type SDecide :: Type -> Constraint
class SDecide k where
  -- | Compute a proof or disproof of equality, given two singletons.
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)
  infix 4 %~

-- | A suitable default implementation for 'testEquality' that leverages
-- 'SDecide'.
decideEquality :: forall k (a :: k) (b :: k). SDecide k
               => Sing a -> Sing b -> Maybe (a :~: b)
decideEquality a b =
  case a %~ b of
    Proved Refl -> Just Refl
    Disproved _ -> Nothing

instance SDecide k => TestEquality (WrappedSing @k) where
  testEquality (WrapSing s1) (WrapSing s2) = decideEquality s1 s2

-- | A suitable default implementation for 'testCoercion' that leverages
-- 'SDecide'.
decideCoercion :: forall k (a :: k) (b :: k). SDecide k
               => Sing a -> Sing b -> Maybe (Coercion a b)
decideCoercion a b =
  case a %~ b of
    Proved Refl -> Just Coercion
    Disproved _ -> Nothing

instance SDecide k => TestCoercion (WrappedSing @k) where
  testCoercion (WrapSing s1) (WrapSing s2) = decideCoercion s1 s2
