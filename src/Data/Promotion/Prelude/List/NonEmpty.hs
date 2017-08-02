{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.List.NonEmpty
-- Copyright   :  (C) 2016 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to promoting 'NonEmpty',
-- including promoted versions of many of the definitions in @Data.List.NonEmpty@.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.List.NonEmpty (

  -- * Non-empty stream transformations
  Map,
  Intersperse,
  Scanl,
  Scanr,
  Scanl1,
  Scanr1,
  Transpose,
  SortBy,
  SortWith,
  Length,
  Head,
  Tail,
  Last,
  Init,
  type (<|),
  Cons,
  Uncons,
  Unfoldr,
  Sort,
  Reverse,
  Inits,
  Tails,
  Unfold,
  Insert,
  Take,
  Drop,
  SplitAt,
  TakeWhile,
  DropWhile,
  Span,
  Break,
  Filter,
  Partition,
  Group,
  GroupBy,
  GroupWith,
  GroupAllWith,
  Group1,
  GroupBy1,
  GroupWith1,
  GroupAllWith1,
  IsPrefixOf,
  Nub,
  NubBy,
  type (!!),
  Zip,
  ZipWith,
  Unzip,
  FromList,
  ToList,
  NonEmpty_,
  Xor,

  -- * Defunctionalization symbols
  (:|@#@$), (:|@#@$$), (:|@#@$$$),
  MapSym0, MapSym1, MapSym2,
  IntersperseSym0, IntersperseSym1, IntersperseSym2,
  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,
  TransposeSym0, TransposeSym1,
  SortBySym0, SortBySym1, SortBySym2,
  SortWithSym0, SortWithSym1, SortWithSym2,
  LengthSym0, LengthSym1,
  HeadSym0, HeadSym1,
  TailSym0, TailSym1,
  LastSym0, LastSym1,
  InitSym0, InitSym1,
  type (<|@#@$), type (<|@#@$$), type (<|@#@$$$),
  ConsSym0, ConsSym1, ConsSym2,
  UnconsSym0, UnconsSym1,
  UnfoldrSym0, UnfoldrSym1, UnfoldrSym2,
  SortSym0, SortSym1,
  ReverseSym0, ReverseSym1,
  InitsSym0, InitsSym1,
  TailsSym0, TailsSym1,
  UnfoldSym0, UnfoldSym1,
  InsertSym0, InsertSym1, InsertSym2,
  TakeSym0, TakeSym1, TakeSym2,
  DropSym0, DropSym1, DropSym2,
  SplitAtSym0, SplitAtSym1, SplitAtSym2,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  FilterSym0, FilterSym1, FilterSym2,
  PartitionSym0, PartitionSym1, PartitionSym2,
  GroupSym0, GroupSym1,
  GroupBySym0, GroupBySym1, GroupBySym2,
  GroupWithSym0, GroupWithSym1, GroupWithSym2,
  GroupAllWithSym0, GroupAllWithSym1, GroupAllWithSym2,
  Group1Sym0, Group1Sym1,
  GroupBy1Sym0, GroupBy1Sym1, GroupBy1Sym2,
  GroupWith1Sym0, GroupWith1Sym1, GroupWith1Sym2,
  GroupAllWith1Sym0, GroupAllWith1Sym1, GroupAllWith1Sym2,
  IsPrefixOfSym0, IsPrefixOfSym1, IsPrefixOfSym2,
  NubSym0, NubSym1,
  NubBySym0, NubBySym1, NubBySym2,
  type (!!@#@$), type (!!@#@$$), type (!!@#@$$$),
  ZipSym0, ZipSym1, ZipSym2,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  UnzipSym0, UnzipSym1,
  FromListSym0, FromListSym1,
  ToListSym0, ToListSym1,
  NonEmpty_Sym0, NonEmpty_Sym1,
  XorSym0, XorSym1
  ) where

import Data.Singletons.Prelude.List.NonEmpty
