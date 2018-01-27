{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Ord
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Provides promoted definitions related to type-level comparisons.
--
-----------------------------------------------------------------------------

module Data.Promotion.Prelude.Ord (
  POrd(..),

  Comparing,
  ThenCmp,

  -- ** Defunctionalization symbols
  ThenCmpSym0, ThenCmpSym1, ThenCmpSym2,
  LTSym0, EQSym0, GTSym0,
  CompareSym0, CompareSym1, CompareSym2,
  type (<@#@$),  type (<@#@$$),  type (<@#@$$$),
  type (<=@#@$), type (<=@#@$$), type (<=@#@$$$),
  type (>@#@$),  type (>@#@$$),  type (>@#@$$$),
  type (>=@#@$), type (>=@#@$$), type (>=@#@$$$),
  MaxSym0, MaxSym1, MaxSym2,
  MinSym0, MinSym1, MinSym2,
  ComparingSym0, ComparingSym1, ComparingSym2, ComparingSym3,
  DownSym0, DownSym1
  ) where

import Data.Singletons.Prelude.Ord
