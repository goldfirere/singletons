-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Singletons
-- Copyright   :  (C) 2013-2014 Richard Eisenberg, Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for '[]',
-- including singled versions of a few of the definitions in @Data.List@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.List@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.List.Singletons (
  -- * The singleton for lists
  Sing, SList(..),

  -- * Basic functions
  type (++), (%++), Head, sHead, Last, sLast, Tail, sTail, Init, sInit,
  Null, sNull, Length, sLength,

   -- * List transformations
  Map, sMap, Reverse, sReverse, Intersperse, sIntersperse,
  Intercalate, sIntercalate, Transpose, sTranspose,
  Subsequences, sSubsequences, Permutations, sPermutations,

  -- * Reducing lists (folds)
  Foldl, sFoldl, Foldl', sFoldl', Foldl1, sFoldl1, Foldl1', sFoldl1',
  Foldr, sFoldr, Foldr1, sFoldr1,

  -- ** Special folds
  Concat, sConcat, ConcatMap, sConcatMap,
  And, sAnd, Or, sOr, Any, sAny, All, sAll,
  Sum, sSum, Product, sProduct, Maximum, sMaximum,
  Minimum, sMinimum,

  -- * Building lists

  -- ** Scans
  Scanl, sScanl, Scanl1, sScanl1, Scanr, sScanr, Scanr1, sScanr1,

  -- ** Accumulating maps
  MapAccumL, sMapAccumL, MapAccumR, sMapAccumR,

  -- ** Cyclical lists
  Replicate, sReplicate,

  -- ** Unfolding
  Unfoldr, sUnfoldr,

  -- * Sublists

  -- ** Extracting sublists
  Take, sTake, Drop, sDrop, SplitAt, sSplitAt,
  TakeWhile, sTakeWhile, DropWhile, sDropWhile, DropWhileEnd, sDropWhileEnd,
  Span, sSpan, Break, sBreak,
  StripPrefix,
  Group, sGroup,
  Inits, sInits, Tails, sTails,

  -- ** Predicates
  IsPrefixOf, sIsPrefixOf, IsSuffixOf, sIsSuffixOf, IsInfixOf, sIsInfixOf,

  -- * Searching lists

  -- ** Searching by equality
  Elem, sElem, NotElem, sNotElem, Lookup, sLookup,

  -- ** Searching with a predicate
  Find, sFind, Filter, sFilter, Partition, sPartition,

  -- * Indexing lists
  type (!!), (%!!),
  ElemIndex, sElemIndex, ElemIndices, sElemIndices,
  FindIndex, sFindIndex, FindIndices, sFindIndices,

  -- * Zipping and unzipping lists
  Zip, sZip, Zip3, sZip3,
  Zip4, Zip5, Zip6, Zip7,
  ZipWith, sZipWith, ZipWith3, sZipWith3,
  ZipWith4, ZipWith5, ZipWith6, ZipWith7,
  Unzip, sUnzip, Unzip3, sUnzip3, Unzip4, sUnzip4,
  Unzip5, sUnzip5, Unzip6, sUnzip6, Unzip7, sUnzip7,

  -- * Special lists

  -- ** Functions on 'Symbol's
  Unlines, sUnlines,
  Unwords, sUnwords,

  -- ** \"Set\" operations
  Nub, sNub, Delete, sDelete, type (\\), (%\\),
  Union, sUnion, Intersect, sIntersect,

  -- ** Ordered lists
  Insert, sInsert, Sort, sSort,

  -- * Generalized functions

  -- ** The \"@By@\" operations

  -- *** User-supplied equality (replacing an @Eq@ context)
  -- | The predicate is assumed to define an equivalence.
  NubBy, sNubBy,
  DeleteBy, sDeleteBy, DeleteFirstsBy, sDeleteFirstsBy,
  UnionBy, sUnionBy, IntersectBy, sIntersectBy,
  GroupBy, sGroupBy,

  -- *** User-supplied comparison (replacing an @Ord@ context)
  -- | The function is assumed to define a total ordering.
  SortBy, sSortBy, InsertBy, sInsertBy,
  MaximumBy, sMaximumBy, MinimumBy, sMinimumBy,

  -- ** The \"@generic@\" operations
  -- | The prefix \`@generic@\' indicates an overloaded function that
  -- is a generalized version of a "Prelude" function.
  GenericLength, sGenericLength,

  -- * Defunctionalization symbols
  NilSym0,
  (:@#@$), (:@#@$$), (:@#@$$$),

  type (++@#@$$$), type (++@#@$$), type (++@#@$),
  HeadSym0, HeadSym1, LastSym0, LastSym1,
  TailSym0, TailSym1, InitSym0, InitSym1, NullSym0, NullSym1,
  LengthSym0, LengthSym1,

  MapSym0, MapSym1, MapSym2, ReverseSym0, ReverseSym1,
  IntersperseSym0, IntersperseSym1, IntersperseSym2,
  IntercalateSym0, IntercalateSym1, IntercalateSym2,
  TransposeSym0, TransposeSym1,
  SubsequencesSym0, SubsequencesSym1,
  PermutationsSym0, PermutationsSym1,

  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldl'Sym0, Foldl'Sym1, Foldl'Sym2, Foldl'Sym3,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
  Foldl1'Sym0, Foldl1'Sym1, Foldl1'Sym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  Foldr1Sym0, Foldr1Sym1, Foldr1Sym2,

  ConcatSym0, ConcatSym1,
  ConcatMapSym0, ConcatMapSym1, ConcatMapSym2,
  AndSym0, AndSym1, OrSym0, OrSym1,
  AnySym0, AnySym1, AnySym2,
  AllSym0, AllSym1, AllSym2,
  SumSym0, SumSym1,
  ProductSym0, ProductSym1,
  MaximumSym0, MaximumSym1,
  MinimumSym0, MinimumSym1,

  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,

  MapAccumLSym0, MapAccumLSym1, MapAccumLSym2, MapAccumLSym3,
  MapAccumRSym0, MapAccumRSym1, MapAccumRSym2, MapAccumRSym3,

  ReplicateSym0, ReplicateSym1, ReplicateSym2,

  UnfoldrSym0, UnfoldrSym1, UnfoldrSym2,

  TakeSym0, TakeSym1, TakeSym2,
  DropSym0, DropSym1, DropSym2,
  SplitAtSym0, SplitAtSym1, SplitAtSym2,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  DropWhileEndSym0, DropWhileEndSym1, DropWhileEndSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  StripPrefixSym0, StripPrefixSym1, StripPrefixSym2,
  GroupSym0, GroupSym1,
  InitsSym0, InitsSym1, TailsSym0, TailsSym1,

  IsPrefixOfSym0, IsPrefixOfSym1, IsPrefixOfSym2,
  IsSuffixOfSym0, IsSuffixOfSym1, IsSuffixOfSym2,
  IsInfixOfSym0, IsInfixOfSym1, IsInfixOfSym2,

  ElemSym0, ElemSym1, ElemSym2,
  NotElemSym0, NotElemSym1, NotElemSym2,
  LookupSym0, LookupSym1, LookupSym2,

  FindSym0, FindSym1, FindSym2,
  FilterSym0, FilterSym1, FilterSym2,
  PartitionSym0, PartitionSym1, PartitionSym2,

  type (!!@#@$), type (!!@#@$$), type (!!@#@$$$),
  ElemIndexSym0, ElemIndexSym1, ElemIndexSym2,
  ElemIndicesSym0, ElemIndicesSym1, ElemIndicesSym2,
  FindIndexSym0, FindIndexSym1, FindIndexSym2,
  FindIndicesSym0, FindIndicesSym1, FindIndicesSym2,

  ZipSym0, ZipSym1, ZipSym2,
  Zip3Sym0, Zip3Sym1, Zip3Sym2, Zip3Sym3,
  Zip4Sym0, Zip4Sym1, Zip4Sym2, Zip4Sym3, Zip4Sym4,
  Zip5Sym0, Zip5Sym1, Zip5Sym2, Zip5Sym3, Zip5Sym4, Zip5Sym5,
  Zip6Sym0, Zip6Sym1, Zip6Sym2, Zip6Sym3, Zip6Sym4, Zip6Sym5, Zip6Sym6,
  Zip7Sym0, Zip7Sym1, Zip7Sym2, Zip7Sym3, Zip7Sym4, Zip7Sym5, Zip7Sym6, Zip7Sym7,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3, ZipWith3Sym4,
  ZipWith4Sym0, ZipWith4Sym1, ZipWith4Sym2, ZipWith4Sym3, ZipWith4Sym4, ZipWith4Sym5,
  ZipWith5Sym0, ZipWith5Sym1, ZipWith5Sym2, ZipWith5Sym3, ZipWith5Sym4, ZipWith5Sym5, ZipWith5Sym6,
  ZipWith6Sym0, ZipWith6Sym1, ZipWith6Sym2, ZipWith6Sym3, ZipWith6Sym4, ZipWith6Sym5, ZipWith6Sym6, ZipWith6Sym7,
  ZipWith7Sym0, ZipWith7Sym1, ZipWith7Sym2, ZipWith7Sym3, ZipWith7Sym4, ZipWith7Sym5, ZipWith7Sym6, ZipWith7Sym7, ZipWith7Sym8,
  UnzipSym0, UnzipSym1,
  Unzip3Sym0, Unzip3Sym1,
  Unzip4Sym0, Unzip4Sym1,
  Unzip5Sym0, Unzip5Sym1,
  Unzip6Sym0, Unzip6Sym1,
  Unzip7Sym0, Unzip7Sym1,

  UnlinesSym0, UnlinesSym1,
  UnwordsSym0, UnwordsSym1,

  NubSym0, NubSym1,
  DeleteSym0, DeleteSym1, DeleteSym2,
  type (\\@#@$), type (\\@#@$$), type (\\@#@$$$),
  UnionSym0, UnionSym1, UnionSym2,
  IntersectSym0, IntersectSym1, IntersectSym2,

  InsertSym0, InsertSym1, InsertSym2,
  SortSym0, SortSym1,

  NubBySym0, NubBySym1, NubBySym2,
  DeleteBySym0, DeleteBySym1, DeleteBySym2, DeleteBySym3,
  DeleteFirstsBySym0, DeleteFirstsBySym1, DeleteFirstsBySym2, DeleteFirstsBySym3,
  UnionBySym0, UnionBySym1, UnionBySym2, UnionBySym3,
  IntersectBySym0, IntersectBySym1, IntersectBySym2, IntersectBySym3,
  GroupBySym0, GroupBySym1, GroupBySym2,

  SortBySym0, SortBySym1, SortBySym2,
  InsertBySym0, InsertBySym1, InsertBySym2, InsertBySym3,
  MaximumBySym0, MaximumBySym1, MaximumBySym2,
  MinimumBySym0, MinimumBySym1, MinimumBySym2,

  GenericLengthSym0, GenericLengthSym1
  ) where

import Data.Foldable.Singletons
import Data.Singletons.Base.Instances
       (Sing, SList(..), NilSym0, type (:@#@$), type (:@#@$$), type (:@#@$$$))
import Data.Traversable.Singletons
import GHC.Base.Singletons
       ( Map, MapSym0, MapSym1, MapSym2, sMap
       , type (++), type (++@#@$), type (++@#@$$), type (++@#@$$$), (%++)
       )

import Data.List.Singletons.Internal
  hiding ( All, AllSym0, AllSym1, AllSym2, sAll
         , And, AndSym0, AndSym1, sAnd
         , Any, AnySym0, AnySym1, AnySym2, sAny
         , Concat, ConcatSym0, ConcatSym1, sConcat
         , ConcatMap, ConcatMapSym0, ConcatMapSym1, ConcatMapSym2, sConcatMap
         , Elem, ElemSym0, ElemSym1, ElemSym2, sElem
         , Find, FindSym0, FindSym1, FindSym2, sFind
         , Foldl1, Foldl1Sym0, Foldl1Sym1, Foldl1Sym2, sFoldl1
         , Foldl', Foldl'Sym0, Foldl'Sym1, Foldl'Sym2, Foldl'Sym3, sFoldl'
         , Foldr1, Foldr1Sym0, Foldr1Sym1, Foldr1Sym2, sFoldr1
         , MapAccumL, MapAccumLSym0, MapAccumLSym1, MapAccumLSym2, MapAccumLSym3, sMapAccumL
         , MapAccumR, MapAccumRSym0, MapAccumRSym1, MapAccumRSym2, MapAccumRSym3, sMapAccumR
         , Maximum, MaximumSym0, MaximumSym1, sMaximum
         , MaximumBy, MaximumBySym0, MaximumBySym1, MaximumBySym2, sMaximumBy
         , Minimum, MinimumSym0, MinimumSym1, sMinimum
         , MinimumBy, MinimumBySym0, MinimumBySym1, MinimumBySym2, sMinimumBy
         , Length, LengthSym0, LengthSym1, sLength
         , NotElem, NotElemSym0, NotElemSym1, NotElemSym2, sNotElem
         , Null, NullSym0, NullSym1, sNull
         , Or, OrSym0, OrSym1, sOr
         , Product, ProductSym0, ProductSym1, sProduct
         , Sum, SumSym0, SumSym1, sSum
         )
