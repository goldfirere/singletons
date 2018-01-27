{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Semigroup
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a promoted version of 'Semigroup'.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Semigroup (
  PSemigroup(..),

  GetMin, GetMax, GetFirst, GetLast, GetDual,
  GetAll, GetAny, GetSum, GetProduct, GetOption,

  Option_,

  -- ** Defunctionalization symbols
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  SconcatSym0, SconcatSym1,
  MinSym0, MinSym1, GetMinSym0, GetMinSym1,
  MaxSym0, MaxSym1, GetMaxSym0, GetMaxSym1,
  FirstSym0, FirstSym1, GetFirstSym0, GetFirstSym1,
  LastSym0, LastSym1, GetLastSym0, GetLastSym1,
  WrapMonoidSym0, WrapMonoidSym1, UnwrapMonoidSym0, UnwrapMonoidSym1,
  DualSym0, DualSym1, GetDualSym0, GetDualSym1,
  AllSym0, AllSym1, GetAllSym0, GetAllSym1,
  AnySym0, AnySym1, GetAnySym0, GetAnySym1,
  SumSym0, SumSym1, GetSumSym0, GetSumSym1,
  ProductSym0, ProductSym1, GetProductSym0, GetProductSym1,
  OptionSym0, OptionSym1, GetOptionSym0, GetOptionSym1,
  ArgSym0, ArgSym1, ArgSym2
  ) where

import Data.Singletons.Prelude.Semigroup
