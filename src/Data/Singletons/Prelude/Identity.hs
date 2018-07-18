{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Identity
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Identity' data type.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Identity (
  -- * The 'Identity' singleton
  Sing(SIdentity, sRunIdentity),
  SIdentity, RunIdentity,

  -- * Defunctionalization symbols
  IdentitySym0, IdentitySym1,
  RunIdentitySym0, RunIdentitySym1
  ) where

import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Functor.Identity
import Data.Singletons.Prelude.Base hiding (Foldr, FoldrSym0, sFoldr)
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Foldable
import Data.Singletons.Prelude.Instances hiding (Foldl, sFoldl)
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Semigroup.Internal
import Data.Singletons.Prelude.Show
import Data.Singletons.Single

$(singletonsOnly [d|
  -- deriving instance Enum a => Enum (Identity a)
  instance Enum a => Enum (Identity a) where
    succ (Identity x)     = Identity (succ x)
    pred (Identity x)     = Identity (pred x)
    toEnum i              = Identity (toEnum i)
    fromEnum (Identity x) = fromEnum x
    enumFromTo (Identity x) (Identity y) = map Identity (enumFromTo   x y)
    enumFromThenTo (Identity x) (Identity y) (Identity z) =
        map Identity (enumFromThenTo x y z)

  -- deriving instance Monoid a => Monoid (Identity a)
  instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

  -- deriving instance Num a => Num (Identity a)
  instance Num a => Num (Identity a) where
    Identity x + Identity y = Identity (x + y)
    Identity x - Identity y = Identity (x - y)
    Identity x * Identity y = Identity (x * y)
    negate (Identity x)     = Identity (negate x)
    abs    (Identity x)     = Identity (abs    x)
    signum (Identity x)     = Identity (signum x)
    fromInteger n           = Identity (fromInteger n)

  -- deriving instance Semigroup a => Semigroup (Identity a)
  instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

  -- -| This instance would be equivalent to the derived instances of the
  -- 'Identity' newtype if the 'runIdentity' field were removed
  instance Show a => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
      showString "Identity " . showsPrec 11 x

  deriving instance Functor Identity

  instance Foldable Identity where
      foldMap f (Identity x)  = f x

      elem x (Identity y)     = x == y
      foldl f z (Identity x)  = f z x
      foldl' f z (Identity x) = f z x
      foldl1 _ (Identity x)   = x
      foldr f z (Identity x)  = f x z
      foldr'                  = foldr
      foldr1 _ (Identity x)   = x
      length _                = 1
      maximum (Identity x)    = x
      minimum (Identity x)    = x
      null _                  = False
      product (Identity x)    = x
      sum (Identity x)        = x
      toList (Identity x)     = [x]

  instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)
    liftA2 f (Identity x) (Identity y) = Identity (f x y)

  instance Monad Identity where
    Identity m >>= k = k m
  |])
