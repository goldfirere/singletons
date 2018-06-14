-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Foldable
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of the 'Foldable' type class.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Foldable (
  PFoldable(..),

  FoldrM, FoldlM,
  Traverse_, For_, SequenceA_, Asum,
  MapM_, ForM_, Sequence_, Msum,
  Concat, ConcatMap, And, Or, Any, All, MaximumBy, MinimumBy,
  NotElem, Find,

  -- * Defunctionalization symbols
  FoldSym0, FoldSym1,
  FoldMapSym0, FoldMapSym1, FoldMapSym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  Foldr'Sym0, Foldr'Sym1, Foldr'Sym2, Foldr'Sym3,
  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldl'Sym0, Foldl'Sym1, Foldl'Sym2, Foldl'Sym3,
  Foldr1Sym0, Foldr1Sym1, Foldr1Sym2,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
  ToListSym0, ToListSym1,
  NullSym0, NullSym1,
  LengthSym0, LengthSym1,
  ElemSym0, ElemSym1, ElemSym2,
  MaximumSym0, MaximumSym1,
  MinimumSym0, MinimumSym1,
  SumSym0, SumSym1,
  ProductSym0, ProductSym1,

  FoldrMSym0, FoldrMSym1, FoldrMSym2, FoldrMSym3,
  FoldlMSym0, FoldlMSym1, FoldlMSym2, FoldlMSym3,

  Traverse_Sym0, Traverse_Sym1, Traverse_Sym2,
  For_Sym0, For_Sym1, For_Sym2,
  SequenceA_Sym0, SequenceA_Sym1,
  AsumSym0, AsumSym1,

  MapM_Sym0, MapM_Sym1, MapM_Sym2,
  ForM_Sym0, ForM_Sym1, ForM_Sym2,
  Sequence_Sym0, Sequence_Sym1,
  MsumSym0, MsumSym1,

  ConcatSym0, ConcatSym1,
  ConcatMapSym0, ConcatMapSym1, ConcatMapSym2,
  AndSym0, AndSym1,
  OrSym0, OrSym1,
  AnySym0, AnySym1, AnySym2,
  AllSym0, AllSym1, AllSym2,
  MaximumBySym0, MaximumBySym1, MaximumBySym2,
  MinimumBySym0, MinimumBySym1, MinimumBySym2,

  NotElemSym0, NotElemSym1, NotElemSym2,
  FindSym0, FindSym1, FindSym2
  ) where

import Data.Singletons.Prelude.Foldable
