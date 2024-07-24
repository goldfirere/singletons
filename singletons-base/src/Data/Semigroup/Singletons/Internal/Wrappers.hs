{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Singletons.Internal.Wrappers
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the @newtype@ wrappers from
-- "Data.Semigroup", all of which are reexported from the "Data.Semigroup"
-- module or imported directly by some other modules.
--
-- This module exists to avoid import cycles with
-- "Data.Monoid.Singletons".
--
----------------------------------------------------------------------------

module Data.Semigroup.Singletons.Internal.Wrappers where

import Control.Monad.Singletons.Internal
import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Ord.Singletons hiding (MinSym0, MinSym1, MaxSym0, MaxSym1)
import Data.Semigroup (Dual(..), All(..), Any(..), Sum(..), Product(..))
import Data.Semigroup.Singletons.Internal.Classes
import Data.Singletons.Base.Enum
import Data.Singletons.Base.Instances
import Data.Singletons.Base.Util
import Data.Singletons.TH
import GHC.Num.Singletons

$(genSingletons        semigroupBasicTypes)
$(singBoundedInstances semigroupBasicTypes)
$(singEqInstances      semigroupBasicTypes)
$(singDecideInstances  semigroupBasicTypes)
$(singOrdInstances     semigroupBasicTypes)

$(singletonsOnly [d|
  instance Applicative Dual where
    pure = Dual
    Dual f <*> Dual x = Dual (f x)

  deriving instance Functor Dual

  instance Monad Dual where
    Dual a >>= k = k a

  instance Semigroup a => Semigroup (Dual a) where
          Dual a <> Dual b = Dual (b <> a)

  instance Semigroup All where
          All a <> All b = All (a && b)

  instance Semigroup Any where
          Any a <> Any b = Any (a || b)

  instance Applicative Sum where
    pure = Sum
    Sum f <*> Sum x = Sum (f x)

  deriving instance Functor Sum

  instance Monad Sum where
    Sum a >>= k = k a

  instance Num a => Semigroup (Sum a) where
          Sum a <> Sum b = Sum (a + b)

  -- deriving newtype instance Num a => Num (Sum a)
  instance Num a => Num (Sum a) where
      Sum a + Sum b = Sum (a + b)
      Sum a - Sum b = Sum (a - b)
      Sum a * Sum b = Sum (a * b)
      negate (Sum a) = Sum (negate a)
      abs    (Sum a) = Sum (abs a)
      signum (Sum a) = Sum (signum a)
      fromInteger n  = Sum (fromInteger n)

  instance Applicative Product where
    pure = Product
    Product f <*> Product x = Product (f x)

  deriving instance Functor Product

  instance Monad Product where
    Product a >>= k = k a

  instance Num a => Semigroup (Product a) where
          Product a <> Product b = Product (a * b)

  -- deriving newtype instance Num a => Num (Product a)
  instance Num a => Num (Product a) where
      Product a + Product b = Product (a + b)
      Product a - Product b = Product (a - b)
      Product a * Product b = Product (a * b)
      negate (Product a) = Product (negate a)
      abs    (Product a) = Product (abs a)
      signum (Product a) = Product (signum a)
      fromInteger n      = Product (fromInteger n)
  |])
