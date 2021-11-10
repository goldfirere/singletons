{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.Foldable.Singletons hiding (Sum)
import Data.Functor.Singletons
import Data.Functor.Sum
import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Traversable.Singletons

{-
In order to keep the type arguments to Sum poly-kinded and with inferred
specificities, we define the singleton version of Sum, as well as its
defunctionalization symbols, by hand. This is very much in the spirit of the
code in Data.Functor.Const.Singletons. (See the comments above SConst in that
module for more details on this choice.)
-}
type SSum :: Sum f g a -> Type
data SSum :: Sum f g a -> Type where
  SInL :: forall f g a (x :: f a).
          Sing x -> SSum ('InL @f @g @a x)
  SInR :: forall f g a (y :: g a).
          Sing y -> SSum ('InR @f @g @a y)
type instance Sing = SSum
instance SingI x => SingI ('InL x) where
  sing = SInL sing
instance SingI1 'InL where
  liftSing = SInL
instance SingI y => SingI ('InR y) where
  sing = SInR sing
instance SingI1 'InR where
  liftSing = SInR

type InLSym0 :: forall f g a. f a ~> Sum f g a
data InLSym0 z
type instance Apply InLSym0 x = 'InL x
instance SingI InLSym0 where
  sing = singFun1 SInL

type InLSym1 :: forall f g a. f a -> Sum f g a
type family InLSym1 x where
  InLSym1 x = 'InL x

type InRSym0 :: forall f g a. g a ~> Sum f g a
data InRSym0 z
type instance Apply InRSym0 y = 'InR y
instance SingI InRSym0 where
  sing = singFun1 SInR

type InRSym1 :: forall f g a. g a -> Sum f g a
type family InRSym1 x where
  InRSym1 y = 'InR y

$(singletonsOnly [d|
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
