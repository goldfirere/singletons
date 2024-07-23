{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Product.Singletons
-- Copyright   :  (C) 2021 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Product' data type.
--
-----------------------------------------------------------------------------

module Data.Functor.Product.Singletons (
  -- * The 'Product' singleton
  Sing, SProduct(..),

  -- * Defunctionalization symbols
  PairSym0, PairSym1, PairSym2
  ) where

import Control.Applicative
import Control.Applicative.Singletons
import Control.Monad
import Control.Monad.Singletons
import Control.Monad.Zip
import Control.Monad.Zip.Singletons
import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Foldable.Singletons hiding (Product)
import Data.Function.Singletons
import Data.Functor.Product
import Data.Kind
import Data.Monoid.Singletons hiding (SProduct(..))
import Data.Semigroup.Singletons hiding (SProduct(..))
import Data.Singletons.Base.Instances (SList(..), (:@#@$), NilSym0)
import Data.Ord.Singletons
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Data.Traversable.Singletons

$(withOptions defaultOptions{genSingKindInsts = False}
    (genSingletons [''Product]))

$(singletonsOnly [d|
  deriving instance (Eq (f a), Eq (g a)) => Eq (Product f g a)
  deriving instance (Ord (f a), Ord (g a)) => Ord (Product f g a)

  instance (Functor f, Functor g) => Functor (Product f g) where
      fmap f (Pair x y) = Pair (fmap f x) (fmap f y)
      a <$ (Pair x y) = Pair (a <$ x) (a <$ y)

  instance (Foldable f, Foldable g) => Foldable (Product f g) where
      foldMap f (Pair x y) = foldMap f x `mappend` foldMap f y

  instance (Traversable f, Traversable g) => Traversable (Product f g) where
      traverse f (Pair x y) = liftA2 Pair (traverse f x) (traverse f y)

  instance (Applicative f, Applicative g) => Applicative (Product f g) where
      pure x = Pair (pure x) (pure x)
      Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)
      liftA2 f (Pair a b) (Pair x y) = Pair (liftA2 f a x) (liftA2 f b y)

  instance (Monad f, Monad g) => Monad (Product f g) where
      Pair m n >>= f = Pair (m >>= fstP . f) (n >>= sndP . f)
        where
          fstP (Pair a _) = a
          sndP (Pair _ b) = b

  instance (MonadZip f, MonadZip g) => MonadZip (Product f g) where
      mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (mzipWith f x1 x2) (mzipWith f y1 y2)

  -- Note that in the instances below, we explicitly annotate `f` with its kind
  -- (Type -> Type), which is not something that is done in the original base
  -- library. This is because when singletons-th promotes instance declarations,
  -- it omits the instance contexts when generating the helper type families.
  -- This can lead to the helper type families having overly polymorphic kinds.
  -- For example, if the Alternative instance below lacked the explicit
  -- (f :: Type -> Type) kind signature, the generated code would look like:
  --
  --   instance PAlternative (Product f g) where
  --     type Empty = EmptyHelper
  --     ...
  --
  --   type EmptyHelper :: forall f g a. Product f g a
  --
  -- This will result in EmptyHelper having a more polymorphic kind than
  -- intended, since GHC will generalize EmptyHelper's kind to:
  --
  --   type EmptyHelper :: forall {k} (f :: k -> Type) (g :: k -> Type) (a :: k). Product f g a
  --
  -- Annotating `f :: Type -> Type` is a clunky but reliable way of preventing
  -- this. See also Note [Using standalone kind signatures not present in the
  -- base library] in Control.Monad.Singletons.Internal for a similar situation
  -- where class definitions can become overly polymorphic unless given an
  -- explicit kind.
  instance (Alternative f, Alternative g) => Alternative (Product (f :: Type -> Type) g) where
      empty = Pair empty empty
      Pair x1 y1 <|> Pair x2 y2 = Pair (x1 <|> x2) (y1 <|> y2)

  instance (MonadPlus f, MonadPlus g) => MonadPlus (Product (f :: Type -> Type) g) where
      mzero = Pair mzero mzero
      Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `mplus` x2) (y1 `mplus` y2)
  |])
