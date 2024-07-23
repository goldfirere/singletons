{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Compose.Singletons
-- Copyright   :  (C) 2021 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Compose' data type.
--
-----------------------------------------------------------------------------

module Data.Functor.Compose.Singletons (
  -- * The 'Compose' singleton
  Sing, SCompose(..), GetCompose, sGetCompose,

  -- * Defunctionalization symbols
  ComposeSym0, ComposeSym1,
  GetComposeSym0, GetComposeSym1
  ) where

import Control.Applicative
import Control.Applicative.Singletons
import Data.Eq.Singletons
import Data.Foldable.Singletons
import Data.Functor.Compose
import Data.Functor.Singletons
import Data.Ord.Singletons
import Data.Kind
import Data.Semigroup.Singletons
import Data.Singletons.Base.Instances (SList(..), (:@#@$), NilSym0)
import Data.Singletons.TH
import Data.Singletons.TH.Options
import Data.Traversable.Singletons

$(withOptions defaultOptions{genSingKindInsts = False}
    (genSingletons [''Compose]))

$(singletonsOnly [d|
  deriving instance Eq (f (g a)) => Eq (Compose f g a)
  deriving instance Ord (f (g a)) => Ord (Compose f g a)

  -- Note that in the instances below, we explicitly annotate `f` with its kind
  -- (Type -> Type), which is not something that is done in the original base
  -- library. This is because when singletons-th promotes instance declarations,
  -- it omits the instance contexts. As such, the instance declarations (as well
  -- as the associated defunctionalization symbols) would be given overly
  -- polymorphic kinds due to kind generalization, e.g.,
  --
  --   instance PFunctor (Compose (f :: k -> Type) (g :: Type -> k)) where ...
  --
  -- Annotating `f :: Type -> Type` is a clunky but reliable way of preventing
  -- this. See also Note [Using standalone kind signatures not present in the
  -- base library] in Control.Monad.Singletons.Internal for a similar situation
  -- where class definitions can become overly polymorphic unless given an
  -- explicit kind.

  instance (Functor f, Functor g) => Functor (Compose (f :: Type -> Type) g) where
      fmap f (Compose x) = Compose (fmap (fmap f) x)
      a <$ (Compose x) = Compose (fmap (a <$) x)

  instance (Foldable f, Foldable g) => Foldable (Compose (f :: Type -> Type) g) where
      foldMap f (Compose t) = foldMap (foldMap f) t

  instance (Traversable f, Traversable g) => Traversable (Compose (f :: Type -> Type) g) where
      traverse f (Compose t) = Compose <$> traverse (traverse f) t

  instance (Applicative f, Applicative g) => Applicative (Compose (f :: Type -> Type) g) where
      pure x = Compose (pure (pure x))
      Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
      liftA2 f (Compose x) (Compose y) =
        Compose (liftA2 (liftA2 f) x y)

  instance (Alternative f, Applicative g) => Alternative (Compose (f :: Type -> Type) g) where
      empty = Compose empty
      Compose x <|> Compose y = Compose (x <|> y)
  |])
