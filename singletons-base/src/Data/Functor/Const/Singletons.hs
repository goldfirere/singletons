{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Const.Singletons
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports the promoted and singled versions of the 'Const' data type.
--
-----------------------------------------------------------------------------

module Data.Functor.Const.Singletons (
  -- * The 'Const' singleton
  Sing, SConst(..), GetConst, sGetConst,

  -- * Defunctionalization symbols
  ConstSym0, ConstSym1,
  GetConstSym0, GetConstSym1
  ) where

import Control.Applicative
import Control.Monad.Singletons.Internal
import Data.Eq.Singletons
import Data.Foldable.Singletons
import Data.Kind (Type)
import Data.Monoid.Singletons
import Data.Ord.Singletons
import Data.Semigroup.Singletons.Internal
import Data.Singletons
import Data.Singletons.Base.Instances hiding (FoldlSym0, sFoldl)
import Data.Singletons.Base.Enum
import Data.Singletons.TH
import GHC.Base.Singletons
  hiding ( Const, ConstSym0, ConstSym1
         , Foldr, FoldrSym0, sFoldr )
import GHC.Num.Singletons
import Text.Show.Singletons

{-
Const's argument `b` is poly-kinded, and as a result, we have a choice as to
what singleton type to give it. We could use either

1. type SConst :: forall {k :: Type} (a :: Type) (b :: k).    Const a b -> Type
2. type SConst :: forall             (a :: Type) (b :: Type). Const a b -> Type

Option (1) is the more permissive one, so we opt for that. However, singletons-th's
TH machinery does not jive with this option, since the SingKind instance it
tries to generate:

  instance (SingKind a, SingKind b) => SingKind (Const a b) where
    type Demote (Const a b) = Const (Demote a) (Demote b)

Assumes that `b` is of kind Type. Until we get a more reliable story for
poly-kinded Sing instances (see #150), we simply write the singleton type by
hand.
-}
type SConst :: Const a b -> Type
data SConst :: Const a b -> Type where
  SConst :: Sing a -> SConst ('Const a)
type instance Sing = SConst
instance SingKind a => SingKind (Const a b) where
  type Demote (Const a b) = Const (Demote a) b
  fromSing (SConst sa) = Const (fromSing sa)
  toSing (Const a) = withSomeSing a $ SomeSing . SConst
instance SingI a => SingI ('Const a) where
  sing = SConst sing

$(genDefunSymbols [''Const])
instance SingI ConstSym0 where
  sing = singFun1 SConst

$(singletonsOnly [d|
  getConst :: Const a b -> a
  getConst (Const x) = x

  deriving instance Bounded a => Bounded (Const a b)
  deriving instance Eq      a => Eq      (Const a b)
  deriving instance Ord     a => Ord     (Const a b)

  -- deriving instance Enum a => Enum (Const a b)
  instance Enum a => Enum (Const a b) where
    succ (Const x)     = Const (succ x)
    pred (Const x)     = Const (pred x)
    toEnum i           = Const (toEnum i)
    fromEnum (Const x) = fromEnum x
    enumFromTo (Const x) (Const y) = map Const (enumFromTo x y)
    enumFromThenTo (Const x) (Const y) (Const z) =
        map Const (enumFromThenTo x y z)

  -- deriving instance Monoid a => Monoid (Const a b)
  instance Monoid a => Monoid (Const a b) where
    mempty = Const mempty

  -- deriving instance Num a => Num (Const a b)
  instance Num a => Num (Const a b) where
    Const x + Const y = Const (x + y)
    Const x - Const y = Const (x - y)
    Const x * Const y = Const (x * y)
    negate (Const x)  = Const (negate x)
    abs    (Const x)  = Const (abs    x)
    signum (Const x)  = Const (signum x)
    fromInteger n     = Const (fromInteger n)

  -- deriving instance Semigroup a => Semigroup (Const a b)
  instance Semigroup a => Semigroup (Const a b) where
    Const x <> Const y = Const (x <> y)

  -- -| This instance would be equivalent to the derived instances of the
  -- 'Const' newtype if the 'runConst' field were removed
  instance Show a => Show (Const a b) where
      showsPrec d (Const x) = showParen (d > 10) $
                              showString "Const " . showsPrec 11 x

  deriving instance Functor (Const m)
  deriving instance Foldable (Const m)

  instance Monoid m => Applicative (Const m) where
      pure _ = Const mempty
      liftA2 _ (Const x) (Const y) = Const (x `mappend` y)
      Const x <*> Const y = Const (x `mappend` y)
  |])
