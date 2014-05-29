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
import Data.Singletons
import Data.Singletons.Util
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Base
import Data.Singletons.TypeLits

$(promoteOnly [d|
  class Bounded a where
    minBound, maxBound :: a
  |])

class (kproxy ~ 'KProxy) => SBounded (kproxy :: KProxy a) where
   -- defaults could be written quite easily, but would be against spec.
  sMinBound :: Sing (MinBound :: a)
  sMaxBound :: Sing (MaxBound :: a)

$(promoteBoundedInstances boundedBasicTypes)

$(promoteOnly [d|
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

class kproxy ~ 'KProxy => SEnum (kproxy :: KProxy a) where
  sSucc :: forall (x :: a). Sing x -> Sing (Succ x)
  sPred :: forall (x :: a). Sing x -> Sing (Pred x)
  sToEnum :: forall (x :: Nat). Sing x -> Sing (ToEnum x :: a)
  sFromEnum :: forall (x :: a). Sing x -> Sing (FromEnum x)
  sEnumFromTo :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (EnumFromTo x y)
  sEnumFromThenTo :: forall (x :: a) (y :: a) (z :: a).
                     Sing x -> Sing y -> Sing z -> Sing (EnumFromThenTo x y z)

  default sSucc :: forall (x :: a).
                   Succ x ~ (ToEnum (1 :+ (FromEnum x)))
                => Sing x -> Sing (Succ x)
  sSucc x = sToEnum ((sing :: Sing 1) %:+ (sFromEnum x))

  default sPred :: forall (x :: a).
                   Pred x ~ (ToEnum (FromEnum x :- 1))
                => Sing x -> Sing (Pred x)
  sPred x = sToEnum (sFromEnum x %:- (sing :: Sing 1))

  -- The following definitions require an Enum instance for Nat. We could
  -- write this today, but it would be painful. Instead, wait for #25 to
  -- be resolved.

  -- default sEnumFromTo :: forall (x :: a) (y :: a).
  --         EnumFromTo x y ~ Map ToEnumSym0 (EnumFromTo (FromEnum x) (FromEnum y))
  --      => Sing x -> Sing y -> Sing (EnumFromTo x y)
  -- sEnumFromTo x y = sMap (singFun1 (Proxy :: Proxy ToEnumSym0) sToEnum)
  --                        (sEnumFromTo (sFromEnum x) (sFromEnum y))

  -- default sEnumFromThenTo
  --   :: forall (x1 :: a) (x2 :: a) (y :: a).
  --      EnumFromThenTo x1 x2 y ~
  --        Map ToEnumSym0 (EnumFromThenTo (FromEnum x1) (FromEnum x2) (FromEnum y))
  --   => Sing x1 -> Sing x2 -> Sing y -> Sing (EnumFromThenTo x1 x2 y)
  -- sEnumFromThenTo x1 x2 y =
  --   sMap (singFun1 (Proxy :: Proxy ToEnumSym0) sToEnum)
  --        (sEnumFromThenTo (sFromEnum x1) (sFromEnum x2) (sFromEnum y))

-- TODO: Write prelude Enum instances. (Bool, Ordering... any others?)
