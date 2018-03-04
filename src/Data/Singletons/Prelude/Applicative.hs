{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Applicative
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'Applicative' type class.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Applicative (
  PApplicative(..), SApplicative(..),
  PAlternative(..), SAlternative(..),
  -- Sing (SConst, sGetConst), SConst, Const, GetConst,
  type (<$>), (%<$>), type (<$), (%<$), type (<**>), (%<**>),
  LiftA, sLiftA, LiftA3, sLiftA3, Optional, sOptional,

  -- * Defunctionalization symbols
  PureSym0, PureSym1,
  type (<*>@#@$), type (<*>@#@$$), type (<*>@#@$$$),
  type (*>@#@$),  type (*>@#@$$),  type (*>@#@$$$),
  type (<*@#@$),  type (<*@#@$$),  type (<*@#@$$$),
  EmptySym0, type (<|>@#@$), type (<|>@#@$$), type (<|>@#@$$$),
  -- ConstSym0, ConstSym1, GetConstSym0, GetConstSym1,
  type (<$>@#@$),  type (<$>@#@$$),  type (<$>@#@$$$),
  type (<$@#@$),   type (<$@#@$$),   type (<$@#@$$$),
  type (<**>@#@$), type (<**>@#@$$), type (<**>@#@$$$),
  LiftASym0,  LiftASym1,  LiftASym2,
  LiftA2Sym0, LiftA2Sym1, LiftA2Sym2, LiftA2Sym3,
  LiftA3Sym0, LiftA3Sym1, LiftA3Sym2, LiftA3Sym3,
  OptionalSym0, OptionalSym1
  ) where

import Control.Applicative
import Data.Ord (Down(..))
import Data.Singletons.Prelude.Functor
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Ord
import Data.Singletons.Single

$(singletonsOnly [d|
  -- -| One or none.
  optional :: Alternative f => f a -> f (Maybe a)
  optional v = Just <$> v <|> pure Nothing

  instance Monoid a => Applicative ((,) a) where
      pure x = (mempty, x)
      (u, f) <*> (v, x) = (u `mappend` v, f x)
      liftA2 f (u, x) (v, y) = (u `mappend` v, f x y)

  instance Applicative Down where
    pure = Down
    Down f <*> Down x = Down (f x)
  |])
