-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Traversable
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of the 'Traversable' type class.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Traversable (
  PTraversable(..),
  For, ForM, MapAccumL, MapAccumR, FmapDefault, FoldMapDefault,

  -- * Defunctionalization symbols
  TraverseSym0, TraverseSym1, TraverseSym2,
  SequenceASym0, SequenceASym1,
  MapMSym0, MapMSym1, MapMSym2,
  SequenceSym0, SequenceSym1,

  ForSym0, ForSym1, ForSym2,
  ForMSym0, ForMSym1, ForMSym2,
  MapAccumLSym0, MapAccumLSym1, MapAccumLSym2, MapAccumLSym3,
  MapAccumRSym0, MapAccumRSym1, MapAccumRSym2, MapAccumRSym3,
  FmapDefaultSym0, FmapDefaultSym1, FmapDefaultSym2,
  FoldMapDefaultSym0, FoldMapDefaultSym1, FoldMapDefaultSym2
  ) where

import Data.Singletons.Prelude.Traversable
