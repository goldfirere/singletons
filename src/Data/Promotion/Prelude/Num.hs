{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Num
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports promoted and singleton versions of definitions from
-- GHC.Num.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Num (
  PNum(..), Subtract,

  -- ** Defunctionalization symbols
  type (+@#@$), type (+@#@$$), type (+@#@$$$),
  type (-@#@$), type (-@#@$$), type (-@#@$$$),
  type (*@#@$), type (*@#@$$), type (*@#@$$$),
  NegateSym0, NegateSym1,
  AbsSym0, AbsSym1,
  SignumSym0, SignumSym1,
  FromIntegerSym0, FromIntegerSym1,
  SubtractSym0, SubtractSym1, SubtractSym2
  ) where

import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits ()   -- for the Num instance!
