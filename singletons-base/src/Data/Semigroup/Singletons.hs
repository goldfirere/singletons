{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Singletons
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of 'Semigroup', 'PSemigroup', and the
-- singleton version, 'SSemigroup'.
--
----------------------------------------------------------------------------

module Data.Semigroup.Singletons (
  PSemigroup(..), SSemigroup(..),

  Sing, SMin(..), SMax(..), SFirst(..), SLast(..),
  SWrappedMonoid(..), SDual(..), SAll(..), SAny(..),
  SSum(..), SProduct(..), SArg(..),
  GetMin, GetMax, GetFirst, GetLast, UnwrapMonoid, GetDual,
  GetAll, GetAny, GetSum, GetProduct,
  sGetMin, sGetMax, sGetFirst, sGetLast, sUnwrapMonoid, sGetDual,
  sGetAll, sGetAny, sGetSum, sGetProduct,

  -- ** Defunctionalization symbols
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  SconcatSym0, SconcatSym1,
  MinSym0, MinSym1, GetMinSym0, GetMinSym1,
  MaxSym0, MaxSym1, GetMaxSym0, GetMaxSym1,
  FirstSym0, FirstSym1, GetFirstSym0, GetFirstSym1,
  LastSym0, LastSym1, GetLastSym0, GetLastSym1,
  WrapMonoidSym0, WrapMonoidSym1, UnwrapMonoidSym0, UnwrapMonoidSym1,
  DualSym0, DualSym1, GetDualSym0, GetDualSym1,
  AllSym0, AllSym1, GetAllSym0, GetAllSym1,
  AnySym0, AnySym1, GetAnySym0, GetAnySym1,
  SumSym0, SumSym1, GetSumSym0, GetSumSym1,
  ProductSym0, ProductSym1, GetProductSym0, GetProductSym1,
  ArgSym0, ArgSym1, ArgSym2
  ) where

import Control.Monad.Singletons.Internal
import Data.Eq.Singletons
import Data.Foldable.Singletons hiding
       ( All,     AllSym0,     AllSym1
       , Any,     AnySym0,     AnySym1
       , Product, ProductSym0, ProductSym1
       , Sum,     SumSym0,     SumSym1 )
import Data.Monoid.Singletons hiding
       (SFirst(..), SLast(..),
        FirstSym0, FirstSym1, LastSym0, LastSym1,
        GetFirst, sGetFirst, GetFirstSym0, GetFirstSym1,
        GetLast,  sGetLast,  GetLastSym0,  GetLastSym1)
import Data.Ord.Singletons hiding
       (MinSym0, MinSym1, MaxSym0, MaxSym1)
import Data.Ord.Singletons.Disambiguation
import qualified Data.Semigroup as Semi (Min(..), Max(..))
import Data.Semigroup (First(..), Last(..), WrappedMonoid(..), Arg(..))
import Data.Semigroup.Singletons.Internal.Classes
import Data.Semigroup.Singletons.Internal.Wrappers
import Data.Singletons.Base.Enum
import Data.Singletons.Base.Instances
import Data.Singletons.Base.Util
import Data.Singletons.TH
import Data.Traversable.Singletons
import GHC.Base.Singletons hiding
       (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFoldr)
import GHC.Num.Singletons
import Text.Show.Singletons

$(genSingletons [''Arg])
$(showSingInstances semigroupBasicTypes)
$(singShowInstances semigroupBasicTypes)

instance Eq (SArg z) where
  _ == _ = True

instance Ord (SArg z) where
  compare _ _ = EQ

$(singletonsOnly [d|
  instance Applicative Semi.Min where
    pure = Semi.Min
    a <* _ = a
    _ *> a = a
    Semi.Min f <*> Semi.Min x = Semi.Min (f x)
    liftA2 f (Semi.Min a) (Semi.Min b) = Semi.Min (f a b)

  instance Enum a => Enum (Semi.Min a) where
    succ (Semi.Min a) = Semi.Min (succ a)
    pred (Semi.Min a) = Semi.Min (pred a)
    toEnum = Semi.Min . toEnum
    fromEnum (Semi.Min a) = fromEnum a
    enumFromTo (Semi.Min a) (Semi.Min b) = Semi.Min `map` enumFromTo a b
    enumFromThenTo (Semi.Min a) (Semi.Min b) (Semi.Min c) = Semi.Min `map` enumFromThenTo a b c

  deriving instance Functor Semi.Min

  instance Monad Semi.Min where
    (>>) = (*>)
    Semi.Min a >>= f = f a

  instance Ord a => Semigroup (Semi.Min a) where
    Semi.Min a <> Semi.Min b = Semi.Min (a `min_` b)

  instance (Ord a, Bounded a) => Monoid (Semi.Min a) where
    mempty = maxBound

  instance Num a => Num (Semi.Min a) where
    (Semi.Min a) + (Semi.Min b) = Semi.Min (a + b)
    (Semi.Min a) * (Semi.Min b) = Semi.Min (a * b)
    (Semi.Min a) - (Semi.Min b) = Semi.Min (a - b)
    negate (Semi.Min a) = Semi.Min (negate a)
    abs    (Semi.Min a) = Semi.Min (abs a)
    signum (Semi.Min a) = Semi.Min (signum a)
    fromInteger         = Semi.Min . fromInteger

  deriving instance Foldable Semi.Min
  deriving instance Traversable Semi.Min

  instance Applicative Semi.Max where
    pure = Semi.Max
    a <* _ = a
    _ *> a = a
    Semi.Max f <*> Semi.Max x = Semi.Max (f x)
    liftA2 f (Semi.Max a) (Semi.Max b) = Semi.Max (f a b)

  instance Enum a => Enum (Semi.Max a) where
    succ (Semi.Max a) = Semi.Max (succ a)
    pred (Semi.Max a) = Semi.Max (pred a)
    toEnum = Semi.Max . toEnum
    fromEnum (Semi.Max a) = fromEnum a
    enumFromTo (Semi.Max a) (Semi.Max b) = Semi.Max `map` enumFromTo a b
    enumFromThenTo (Semi.Max a) (Semi.Max b) (Semi.Max c) = Semi.Max `map` enumFromThenTo a b c

  deriving instance Functor Semi.Max

  instance Monad Semi.Max where
    (>>) = (*>)
    Semi.Max a >>= f = f a

  instance Ord a => Semigroup (Semi.Max a) where
    Semi.Max a <> Semi.Max b = Semi.Max (a `max_` b)

  instance (Ord a, Bounded a) => Monoid (Semi.Max a) where
    mempty = minBound

  instance Num a => Num (Semi.Max a) where
    (Semi.Max a) + (Semi.Max b) = Semi.Max (a + b)
    (Semi.Max a) * (Semi.Max b) = Semi.Max (a * b)
    (Semi.Max a) - (Semi.Max b) = Semi.Max (a - b)
    negate (Semi.Max a) = Semi.Max (negate a)
    abs    (Semi.Max a) = Semi.Max (abs a)
    signum (Semi.Max a) = Semi.Max (signum a)
    fromInteger         = Semi.Max . fromInteger

  deriving instance Foldable Semi.Max
  deriving instance Traversable Semi.Max

  instance Eq a => Eq (Arg a b) where
    Arg a _ == Arg b _ = a == b

  deriving instance Functor (Arg a)

  instance Ord a => Ord (Arg a b) where
    Arg a _ `compare` Arg b _ = compare a b
    min x@(Arg a _) y@(Arg b _)
      | a <= b    = x
      | otherwise = y
    max x@(Arg a _) y@(Arg b _)
      | a >= b    = x
      | otherwise = y

  deriving instance (Show a, Show b) => Show (Arg a b)
  deriving instance Foldable (Arg a)
  deriving instance Traversable (Arg a)

  instance Applicative First where
    pure x = First x
    a <* _ = a
    _ *> a = a
    First f <*> First x = First (f x)
    liftA2 f (First a) (First b) = First (f a b)

  instance Enum a => Enum (First a) where
    succ (First a) = First (succ a)
    pred (First a) = First (pred a)
    toEnum = First . toEnum
    fromEnum (First a) = fromEnum a
    enumFromTo (First a) (First b) = First `map` enumFromTo a b
    enumFromThenTo (First a) (First b) (First c) = First `map` enumFromThenTo a b c

  deriving instance Functor First

  instance Monad First where
    (>>) = (*>)
    First a >>= f = f a

  instance Semigroup (First a) where
    a <> _ = a

  deriving instance Foldable First
  deriving instance Traversable First

  instance Applicative Last where
    pure x = Last x
    a <* _ = a
    _ *> a = a
    Last f <*> Last x = Last (f x)
    liftA2 f (Last a) (Last b) = Last (f a b)

  instance Enum a => Enum (Last a) where
    succ (Last a) = Last (succ a)
    pred (Last a) = Last (pred a)
    toEnum = Last . toEnum
    fromEnum (Last a) = fromEnum a
    enumFromTo (Last a) (Last b) = Last `map` enumFromTo a b
    enumFromThenTo (Last a) (Last b) (Last c) = Last `map` enumFromThenTo a b c

  deriving instance Functor Last

  instance Monad Last where
    (>>) = (*>)
    Last a >>= f = f a

  instance Semigroup (Last a) where
    _ <> b = b

  deriving instance Foldable Last
  deriving instance Traversable Last

  instance Monoid m => Semigroup (WrappedMonoid m) where
    WrapMonoid a <> WrapMonoid b = WrapMonoid (a `mappend` b)

  instance Monoid m => Monoid (WrappedMonoid m) where
    mempty = WrapMonoid mempty

  instance Enum a => Enum (WrappedMonoid a) where
    succ (WrapMonoid a) = WrapMonoid (succ a)
    pred (WrapMonoid a) = WrapMonoid (pred a)
    toEnum = WrapMonoid . toEnum
    fromEnum (WrapMonoid a) = fromEnum a
    enumFromTo (WrapMonoid a) (WrapMonoid b) = WrapMonoid `map` enumFromTo a b
    enumFromThenTo (WrapMonoid a) (WrapMonoid b) (WrapMonoid c) =
        WrapMonoid `map` enumFromThenTo a b c
  |])
