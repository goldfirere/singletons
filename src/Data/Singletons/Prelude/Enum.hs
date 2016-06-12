{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables,
             TypeFamilies, TypeOperators, GADTs, UndecidableInstances,
             FlexibleContexts, DefaultSignatures, BangPatterns, TypeInType,
             InstanceSigs #-}

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

import Data.Singletons.Single
import Data.Singletons.Util
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.TypeLits

$(singletonsOnly [d|
  class Bounded a where
    minBound, maxBound :: a
  |])

$(singBoundedInstances boundedBasicTypes)

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

      succ                   = toEnum . (1 +)  . fromEnum
      pred                   = toEnum . (subtract 1) . fromEnum
      -- enumFrom x             = map toEnum [fromEnum x ..]
      -- enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
      enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
      enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

  -- Nat instance for Enum
  eftNat :: Nat -> Nat -> [Nat]
  -- [x1..x2]
  eftNat x0 y | (x0 > y)  = []
              | otherwise = go x0
                 where
                   go x = x : if (x == y) then [] else go (x + 1)

  efdtNat :: Nat -> Nat -> Nat -> [Nat]
  -- [x1,x2..y]
  efdtNat x1 x2 y
   | x2 >= x1  = efdtNatUp x1 x2 y
   | otherwise = efdtNatDn x1 x2 y

  -- Requires x2 >= x1
  efdtNatUp :: Nat -> Nat -> Nat -> [Nat]
  efdtNatUp x1 x2 y    -- Be careful about overflow!
   | y < x2    = if y < x1 then [] else [x1]
   | otherwise = -- Common case: x1 <= x2 <= y
                 let delta = x2 - x1 -- >= 0
                     y' = y - delta  -- x1 <= y' <= y; hence y' is representable

                     -- Invariant: x <= y
                     -- Note that: z <= y' => z + delta won't overflow
                     -- so we are guaranteed not to overflow if/when we recurse
                     go_up x | x > y'    = [x]
                             | otherwise = x : go_up (x + delta)
                 in x1 : go_up x2

  -- Requires x2 <= x1
  efdtNatDn :: Nat -> Nat -> Nat -> [Nat]
  efdtNatDn x1 x2 y    -- Be careful about underflow!
   | y > x2    = if y > x1 then [] else [x1]
   | otherwise = -- Common case: x1 >= x2 >= y
                 let delta = x2 - x1 -- <= 0
                     y' = y - delta  -- y <= y' <= x1; hence y' is representable

                     -- Invariant: x >= y
                     -- Note that: z >= y' => z + delta won't underflow
                     -- so we are guaranteed not to underflow if/when we recurse
                     go_dn x | x < y'    = [x]
                             | otherwise = x : go_dn (x + delta)
     in x1 : go_dn x2

  instance  Enum Nat  where
      succ x = x + 1
      pred x = x - 1

      toEnum   x = x
      fromEnum x = x

      enumFromTo = eftNat
      enumFromThenTo = efdtNat
  |])

$(singEnumInstances enumBasicTypes)
