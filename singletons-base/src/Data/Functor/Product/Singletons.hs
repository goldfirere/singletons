{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Foldable.Singletons hiding (Product)
import Data.Function.Singletons
import Data.Functor.Product
import Data.Kind
import Data.Monoid.Singletons hiding (SProduct(..))
import Data.Singletons
import Data.Singletons.TH
import Data.Traversable.Singletons

{-
In order to keep the type arguments to Product poly-kinded and with inferred
specificities, we define the singleton version of Product, as well as its
defunctionalization symbols, by hand. This is very much in the spirit of the
code in Data.Functor.Const.Singletons. (See the comments above SConst in that
module for more details on this choice.)
-}
type SProduct :: Product f g a -> Type
data SProduct :: Product f g a -> Type where
  SPair :: forall f g a (x :: f a) (y :: g a).
           Sing x -> Sing y -> SProduct ('Pair @f @g @a x y)
type instance Sing = SProduct
instance (SingI x, SingI y) => SingI ('Pair x y) where
  sing = SPair sing sing
instance SingI x => SingI1 ('Pair x) where
  liftSing = SPair sing
instance SingI2 'Pair where
  liftSing2 = SPair

type PairSym0 :: forall f g a. f a ~> g a ~> Product f g a
data PairSym0 z
type instance Apply PairSym0 x = PairSym1 x
instance SingI PairSym0 where
  sing = singFun2 SPair

type PairSym1 :: forall f g a. f a -> g a ~> Product f g a
data PairSym1 fa z
type instance Apply (PairSym1 x) y = 'Pair x y
instance SingI x => SingI (PairSym1 x) where
  sing = singFun1 $ SPair (sing @x)
instance SingI1 PairSym1 where
  liftSing s = singFun1 $ SPair s

type PairSym2 :: forall f g a. f a -> g a -> Product f g a
type family PairSym2 x y where
  PairSym2 x y = 'Pair x y

$(singletonsOnly [d|
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

  instance (Alternative f, Alternative g) => Alternative (Product f g) where
      empty = Pair empty empty
      Pair x1 y1 <|> Pair x2 y2 = Pair (x1 <|> x2) (y1 <|> y2)

  instance (Monad f, Monad g) => Monad (Product f g) where
      Pair m n >>= f = Pair (m >>= fstP . f) (n >>= sndP . f)
        where
          fstP (Pair a _) = a
          sndP (Pair _ b) = b

  instance (MonadPlus f, MonadPlus g) => MonadPlus (Product f g) where
      mzero = Pair mzero mzero
      Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `mplus` x2) (y1 `mplus` y2)

  instance (MonadZip f, MonadZip g) => MonadZip (Product f g) where
      mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (mzipWith f x1 x2) (mzipWith f y1 y2)
  |])
