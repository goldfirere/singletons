-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Monoid
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a promoted version of 'Monoid'.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Monoid (
  PMonoid(..),

  GetDual, GetAll, GetAny, GetSum, GetProduct, GetFirst, GetLast,

  -- ** Defunctionalization symbols
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MconcatSym0, MconcatSym1,
  DualSym0, DualSym1, GetDualSym0, GetDualSym1,
  AllSym0, AllSym1, GetAllSym0, GetAllSym1,
  AnySym0, AnySym1, GetAnySym0, GetAnySym1,
  SumSym0, SumSym1, GetSumSym0, GetSumSym1,
  ProductSym0, ProductSym1, GetProductSym0, GetProductSym1,
  FirstSym0, FirstSym1, GetFirstSym0, GetFirstSym1,
  LastSym0, LastSym1, GetLastSym0, GetLastSym1
  ) where

import Data.Singletons.Prelude.Monoid
