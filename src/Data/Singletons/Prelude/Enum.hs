{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables,
             TypeFamilies, TypeOperators, GADTs, UndecidableInstances,
             FlexibleContexts, DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Enum
-- Copyright   :  (C) 2014 Jan Stolarek, Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singleton version of Bounded, 'PBounded'
-- and 'SBounded'
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Enum (
  PBounded(..), SBounded(..),
  PEnum(..), SEnum(..),

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

import Data.Singletons.Promote
import Data.Singletons.Single
import Data.Singletons
import Data.Singletons.Util
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Base
import Data.Singletons.TypeLits

$(singletonsOnly [d|
  class Bounded a where
    minBound, maxBound :: a
  |])

-- TODO: Singletonize Bounded instances
$(promoteBoundedInstances boundedBasicTypes)

$(singletonsOnly [d|
  class  Enum a   where
      -- | the successor of a value.  For numeric types, 'succ' adds 1.
      succ                :: a -> a
      -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
      pred                :: a -> a
      -- | Convert from a 'Nat'.
      toEnum              :: Nat -> a
      -- | Convert to a 'Nat'.
      fromEnum            :: a -> Nat

      -- The following use infinite lists, and are not promotable:
      -- -- | Used in Haskell's translation of @[n..]@.
      -- enumFrom            :: a -> [a]
      -- -- | Used in Haskell's translation of @[n,n'..]@.
      -- enumFromThen        :: a -> a -> [a]

      -- | Used in Haskell's translation of @[n..m]@.
      enumFromTo          :: a -> a -> [a]
      -- | Used in Haskell's translation of @[n,n'..m]@.
      enumFromThenTo      :: a -> a -> a -> [a]

      succ                   = toEnum . (+ 1)  . fromEnum
      pred                   = toEnum . (subtract 1) . fromEnum
      -- enumFrom x             = map toEnum [fromEnum x ..]
      -- enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
      enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
      enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]
  |])

-- TODO: Write prelude Enum instances. (Bool, Ordering... any others?)
