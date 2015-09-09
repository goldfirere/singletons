{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             UndecidableInstances, GADTs #-}

-- Suppress orphan instance warning for PEnum KProxy. This will go away once #25
-- is fixed and instance declaration for Enum Nat is moved to
-- Data.Singletons.Prelude.Enum module.
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Enum
-- Copyright   :  (C) 2014 Jan Stolarek, Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports promoted versions of 'Enum' and 'Bounded'
--
-----------------------------------------------------------------------------

module Data.Promotion.Prelude.Enum (
  PBounded(..), PEnum(..),

  -- ** Defunctionalization symbols
  MinBoundSym0,
  MaxBoundSym0,
  SuccSym0, SuccSym1,
  PredSym0, PredSym1,
  ToEnumSym0, ToEnumSym1,
  FromEnumSym0, FromEnumSym1,
  EnumFromToSym0, EnumFromToSym1, EnumFromToSym2,
  EnumFromThenToSym0, EnumFromThenToSym1, EnumFromThenToSym2,
  EnumFromThenToSym3
  ) where

import Data.Singletons.Prelude.Enum
