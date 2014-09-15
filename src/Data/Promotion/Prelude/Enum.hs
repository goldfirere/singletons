{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             UndecidableInstances #-}

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
import Data.Singletons.Promote
import Data.Singletons.TypeLits
import Data.Promotion.Prelude.Ord
import Data.Promotion.Prelude.Num
import Data.Promotion.Prelude.Eq
import Data.Promotion.Prelude.List

$(promoteOnly [d|
  instance Enum Nat where
    toEnum n | n >= 0    = n
             | otherwise = error "Enumerating negative Nat literals not supported"

    fromEnum n | n >= 0    = n
               | otherwise = error "Enumerating negative Nat literals not supported"

    succ n = 1 + n

    pred 0 = 0
    pred n | n > 0 = n - 1
           | otherwise = error "Negative type-level Nat literals not supported"

    enumFromTo x y | x == y = [x]
                   | x <  y = x : enumFromTo (x + 1) y
                   | x >  y = []

    enumFromThenTo x1 x2 y | x1 == y = [x1]
                           | x1 <  y = x1 : enumFromTo (x1 + x2) y
                           | x1 >  y = []
  |])
