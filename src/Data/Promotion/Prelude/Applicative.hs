{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Applicative
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of the 'Applicative' type class.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Applicative (
  PApplicative(..), PAlternative(..),
  -- Const, GetConst,
  type (<$>), type (<$), type (<**>),
  LiftA, LiftA3, Optional,

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

import Data.Singletons.Prelude.Applicative
