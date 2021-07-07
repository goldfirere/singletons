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
import Data.Foldable.Singletons
import Data.Functor.Compose
import Data.Functor.Singletons
import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Traversable.Singletons

{-
In order to keep the type arguments to Compose poly-kinded and with inferred
specificities, we define the singleton version of Compose, as well as its
defunctionalization symbols, by hand. This is very much in the spirit of the
code in Data.Functor.Const.Singletons. (See the comments above SConst in that
module for more details on this choice.)
-}
infixr 9 `SCompose`
type SCompose :: Compose f g a -> Type
data SCompose :: Compose f g a -> Type where
  SCompose :: forall f g a (x :: f (g a)).
              Sing x -> SCompose ('Compose @f @g @a x)
type instance Sing = SCompose
instance SingI x => SingI ('Compose x) where
  sing = SCompose sing
instance SingI1 'Compose where
  liftSing = SCompose

infixr 9 `ComposeSym0`
type ComposeSym0 :: f (g a) ~> Compose f g a
data ComposeSym0 z
type instance Apply ComposeSym0 x = 'Compose x
instance SingI ComposeSym0 where
  sing = singFun1 SCompose

infixr 9 `ComposeSym1`
type ComposeSym1 :: f (g a) -> Compose f g a
type family ComposeSym1 x where
  ComposeSym1 x = 'Compose x

$(singletonsOnly [d|
  getCompose :: Compose f g a -> f (g a)
  getCompose (Compose x) = x

  instance (Functor f, Functor g) => Functor (Compose f g) where
      fmap f (Compose x) = Compose (fmap (fmap f) x)
      a <$ (Compose x) = Compose (fmap (a <$) x)

  instance (Foldable f, Foldable g) => Foldable (Compose f g) where
      foldMap f (Compose t) = foldMap (foldMap f) t

  instance (Traversable f, Traversable g) => Traversable (Compose f g) where
      traverse f (Compose t) = Compose <$> traverse (traverse f) t

  instance (Applicative f, Applicative g) => Applicative (Compose f g) where
      pure x = Compose (pure (pure x))
      Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
      liftA2 f (Compose x) (Compose y) =
        Compose (liftA2 (liftA2 f) x y)

  instance (Alternative f, Applicative g) => Alternative (Compose f g) where
      empty = Compose empty
      Compose x <|> Compose y = Compose (x <|> y)
  |])
