{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Sum.Singletons
-- Copyright   :  (C) 2021 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Sum' data type.
--
-----------------------------------------------------------------------------

module Data.Functor.Sum.Singletons (
  -- * The 'Product' singleton
  Sing, SSum(..),

  -- * Defunctionalization symbols
  InLSym0, InLSym1,
  InRSym0, InRSym1,
  ) where

import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Foldable.Singletons hiding (Sum)
import Data.Functor.Singletons
import Data.Functor.Sum
import Data.Ord.Singletons
import Data.Semigroup.Singletons hiding (SSum(..))
import Data.Singletons.Base.Instances (SList(..), (:@#@$), NilSym0)
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Data.Traversable.Singletons

$(withOptions defaultOptions{genSingKindInsts = False}
    (genSingletons [''Sum]))

$(singletonsOnly [d|
  deriving instance (Eq (f a), Eq (g a)) => Eq (Sum f g a)
  deriving instance (Ord (f a), Ord (g a)) => Ord (Sum f g a)

  instance (Functor f, Functor g) => Functor (Sum f g) where
      fmap f (InL x) = InL (fmap f x)
      fmap f (InR y) = InR (fmap f y)

      a <$ (InL x) = InL (a <$ x)
      a <$ (InR y) = InR (a <$ y)

  instance (Foldable f, Foldable g) => Foldable (Sum f g) where
      foldMap f (InL x) = foldMap f x
      foldMap f (InR y) = foldMap f y

  instance (Traversable f, Traversable g) => Traversable (Sum f g) where
      traverse f (InL x) = InL <$> traverse f x
      traverse f (InR y) = InR <$> traverse f y
  |])
