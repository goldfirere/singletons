{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the basic definitions to use singletons. For routine
-- use, consider importing 'Data.Singletons.Prelude', which exports constructors
-- for singletons based on types in the @Prelude@.
--
-- You may also want to read
-- the original papers presenting this library, available at
-- <http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf>
-- and <http://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf>.
--
----------------------------------------------------------------------------

module Data.Singletons (
  -- * Main singleton definitions

  Sing(..), (@@),

  SingI(..), SingKind(..),

  -- * Working with singletons
  KindOf, SameKind,
  SingInstance(..), SomeSing(..),
  singInstance, withSingI, withSomeSing, singByProxy,

  singByProxy#,
  withSing, singThat,

  -- ** Defunctionalization
  TyFun, type (~>),
  TyCon1, TyCon2, TyCon3, TyCon4, TyCon5, TyCon6, TyCon7, TyCon8,
  Apply, type (@@),

  -- ** Defunctionalized singletons
  -- | When calling a higher-order singleton function, you need to use a
  -- @singFun...@ function to wrap it. See 'singFun1'.
  singFun1, singFun2, singFun3, singFun4, singFun5, singFun6, singFun7,
  singFun8,
  unSingFun1, unSingFun2, unSingFun3, unSingFun4, unSingFun5,
  unSingFun6, unSingFun7, unSingFun8,

  -- | These type synonyms are exported only to improve error messages; users
  -- should not have to mention them.
  SingFunction1, SingFunction2, SingFunction3, SingFunction4, SingFunction5,
  SingFunction6, SingFunction7, SingFunction8,

  -- * Auxiliary functions
  Proxy(..)
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Num

----------------------------------------------------------------------
---- SomeSing instances ----------------------------------------------
----------------------------------------------------------------------

instance SEq k => Eq (SomeSing k) where
  SomeSing a == SomeSing b = fromSing (a %:== b)
  SomeSing a /= SomeSing b = fromSing (a %:/= b)

instance SOrd k => Ord (SomeSing k) where
  SomeSing a `compare` SomeSing b = fromSing (a `sCompare` b)
  SomeSing a <         SomeSing b = fromSing (a %:<  b)
  SomeSing a <=        SomeSing b = fromSing (a %:<= b)
  SomeSing a >         SomeSing b = fromSing (a %:>  b)
  SomeSing a >=        SomeSing b = fromSing (a %:>= b)

instance SBounded k => Bounded (SomeSing k) where
  minBound = SomeSing sMinBound
  maxBound = SomeSing sMaxBound

instance (SEnum k, SingKind k) => Enum (SomeSing k) where
  succ (SomeSing a) = SomeSing (sSucc a)
  pred (SomeSing a) = SomeSing (sPred a)
  toEnum n = withSomeSing (fromIntegral n) (SomeSing . sToEnum)
  fromEnum (SomeSing a) = fromIntegral (fromSing (sFromEnum a))
  enumFromTo (SomeSing from) (SomeSing to) =
    map toSing (fromSing (sEnumFromTo from to))
  enumFromThenTo (SomeSing from) (SomeSing then_) (SomeSing to) =
    map toSing (fromSing (sEnumFromThenTo from then_ to))

instance SNum k => Num (SomeSing k) where
  SomeSing a + SomeSing b = SomeSing (a %:+ b)
  SomeSing a - SomeSing b = SomeSing (a %:- b)
  SomeSing a * SomeSing b = SomeSing (a %:* b)
  negate (SomeSing a) = SomeSing (sNegate a)
  abs    (SomeSing a) = SomeSing (sAbs a)
  signum (SomeSing a) = SomeSing (sSignum a)
  fromInteger n = withSomeSing n (SomeSing . sFromInteger)
