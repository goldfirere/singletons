{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.SomeSing
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Provides the 'SomeSing' data type along with instances of 'Eq', 'Ord', etc.,
-- which are defined as orphans due to 'SomeSing' originally being defined in
-- a separate library (@singletons@).
--
----------------------------------------------------------------------------
module Data.Singletons.Prelude.SomeSing (SomeSing(..)) where

import Data.Singletons
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.IsString
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.ShowSing
import Data.String
import qualified Data.Text as T (pack)

instance SEq k => Eq (SomeSing k) where
  SomeSing a == SomeSing b = fromSing (a %== b)
  SomeSing a /= SomeSing b = fromSing (a %/= b)

instance SOrd k => Ord (SomeSing k) where
  SomeSing a `compare` SomeSing b = fromSing (a `sCompare` b)
  SomeSing a <         SomeSing b = fromSing (a %<  b)
  SomeSing a <=        SomeSing b = fromSing (a %<= b)
  SomeSing a >         SomeSing b = fromSing (a %>  b)
  SomeSing a >=        SomeSing b = fromSing (a %>= b)

instance SBounded k => Bounded (SomeSing k) where
  minBound = SomeSing sMinBound
  maxBound = SomeSing sMaxBound

instance SEnum k => Enum (SomeSing k) where
  succ (SomeSing a) = SomeSing (sSucc a)
  pred (SomeSing a) = SomeSing (sPred a)
  toEnum n = withSomeSing (fromIntegral n) (SomeSing . sToEnum)
  fromEnum (SomeSing a) = fromIntegral (fromSing (sFromEnum a))
  enumFromTo (SomeSing from) (SomeSing to) =
    listFromSingShallow (sEnumFromTo from to)
  enumFromThenTo (SomeSing from) (SomeSing then_) (SomeSing to) =
    listFromSingShallow (sEnumFromThenTo from then_ to)

-- Like the 'fromSing' implementation for lists, but bottoms out at
-- 'SomeSing' instead of recursively invoking 'fromSing'.
listFromSingShallow :: SList (x :: [a]) -> [SomeSing a]
listFromSingShallow SNil         = []
listFromSingShallow (SCons x xs) = SomeSing x : listFromSingShallow xs

instance SNum k => Num (SomeSing k) where
  SomeSing a + SomeSing b = SomeSing (a %+ b)
  SomeSing a - SomeSing b = SomeSing (a %- b)
  SomeSing a * SomeSing b = SomeSing (a %* b)
  negate (SomeSing a) = SomeSing (sNegate a)
  abs    (SomeSing a) = SomeSing (sAbs a)
  signum (SomeSing a) = SomeSing (sSignum a)
  fromInteger n = withSomeSing (fromIntegral n) (SomeSing . sFromInteger)

instance ShowSing k => Show (SomeSing k) where
  showsPrec p (SomeSing (s :: Sing a)) =
    showParen (p > 10) $ showString "SomeSing " . showsPrec 11 s
      :: ShowSing' a => ShowS

instance SSemigroup k => Semigroup (SomeSing k) where
  SomeSing a <> SomeSing b = SomeSing (a %<> b)

instance SMonoid k => Monoid (SomeSing k) where
  mempty = SomeSing sMempty

instance SIsString k => IsString (SomeSing k) where
  fromString s = withSomeSing (T.pack s) (SomeSing . sFromString)
