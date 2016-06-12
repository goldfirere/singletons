{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeOperators, TypeInType,
             TypeFamilies, FlexibleContexts, UndecidableInstances, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Decide
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
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
  (:~:)(..), Void, Refuted, Decision(..)
  ) where

import Data.Kind
import Data.Singletons
import Data.Type.Equality
import Data.Void

----------------------------------------------------------------------
---- SDecide ---------------------------------------------------------
----------------------------------------------------------------------

-- | Because we can never create a value of type 'Void', a function that type-checks
-- at @a -> Void@ shows that objects of type @a@ can never exist. Thus, we say that
-- @a@ is 'Refuted'
type Refuted a = (a -> Void)

-- | A 'Decision' about a type @a@ is either a proof of existence or a proof that @a@
-- cannot exist.
data Decision a = Proved a               -- ^ Witness for @a@
                | Disproved (Refuted a)  -- ^ Proof that no @a@ exists

-- | Members of the 'SDecide' "kind" class support decidable equality. Instances
-- of this class are generated alongside singleton definitions for datatypes that
-- derive an 'Eq' instance.
class SDecide k where
  -- | Compute a proof or disproof of equality, given two singletons.
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

instance SDecide k => TestEquality (Sing :: k -> Type) where
  testEquality a b =
    case a %~ b of
      Proved Refl -> Just Refl
      Disproved _ -> Nothing
