{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.List
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted functions and datatypes relating to 'List',
-- including a promoted version of all the definitions in @Data.List@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.List@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.List (
  -- * Basic functions
  (:++), Head, Last, Tail, Init, Null, Length,

   -- * List transformations
  Map, Reverse, Intersperse, Intercalate, Transpose, Subsequences, Permutations,

  -- * Reducing lists (folds)
  Foldl, Foldl', Foldl1, Foldl1', Foldr, Foldr1,

  -- ** Special folds
  Concat, ConcatMap, And, Or, Any_, All, Sum, Product, Maximum, Minimum,
  any_, -- equivalent of Data.List `any`. Avoids name clash with Any type

  -- * Building lists

  -- ** Scans
  Scanl, Scanl1, Scanr, Scanr1,

  -- ** Accumulating maps
  MapAccumL, MapAccumR,

  -- ** Infinite lists
  Replicate,

  -- ** Unfolding
  Unfoldr,

  -- * Sublists

  -- ** Extracting sublists
  Take, Drop, SplitAt,
  TakeWhile, DropWhile, DropWhileEnd, Span, Break,
  StripPrefix,
  Group,
  Inits, Tails,

  -- ** Predicates
  IsPrefixOf, IsSuffixOf, IsInfixOf,

  -- * Searching lists

  -- ** Searching by equality
  Elem, NotElem, Lookup,

  -- ** Searching with a predicate
  Find, Filter, Partition,

  -- * Indexing lists
  (:!!), ElemIndex, ElemIndices, FindIndex, FindIndices,

  -- * Zipping and unzipping lists
  Zip, Zip3, Zip4, Zip5, Zip6, Zip7,
  ZipWith, ZipWith3, ZipWith4, ZipWith5, ZipWith6, ZipWith7,
  Unzip, Unzip3, Unzip4, Unzip5, Unzip6, Unzip7,

  -- * Special lists

  -- ** \"Set\" operations
  Nub, Delete, (:\\), Union, Intersect,

  -- ** Ordered lists
  Sort, Insert,

  -- * Generalized functions

  -- ** The \"@By@\" operations
  -- *** User-supplied equality (replacing an @Eq@ context)
  NubBy, DeleteBy, DeleteFirstsBy, UnionBy, GroupBy, IntersectBy,

  -- *** User-supplied comparison (replacing an @Ord@ context)
  SortBy, InsertBy,
  MaximumBy, MinimumBy,

   -- ** The \"@generic@\" operations
  GenericLength, GenericTake, GenericDrop,
  GenericSplitAt, GenericIndex, GenericReplicate,

  -- * Defunctionalization symbols
  NilSym0,
  (:$), (:$$), (:$$$),

  (:++$$$), (:++$$), (:++$), HeadSym0, HeadSym1, LastSym0, LastSym1,
  TailSym0, TailSym1, InitSym0, InitSym1, NullSym0, NullSym1,

  MapSym0, MapSym1, MapSym2, ReverseSym0, ReverseSym1,
  IntersperseSym0, IntersperseSym1, IntersperseSym2,
  IntercalateSym0, IntercalateSym1, IntercalateSym2,
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
  Any_Sym0, Any_Sym1, Any_Sym2,
  AllSym0, AllSym1, AllSym2,

  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,

  MapAccumLSym0, MapAccumLSym1, MapAccumLSym2, MapAccumLSym3,
  MapAccumRSym0, MapAccumRSym1, MapAccumRSym2, MapAccumRSym3,

  UnfoldrSym0, UnfoldrSym1, UnfoldrSym2,

  InitsSym0, InitsSym1, TailsSym0, TailsSym1,

  IsPrefixOfSym0, IsPrefixOfSym1, IsPrefixOfSym2,
  IsSuffixOfSym0, IsSuffixOfSym1, IsSuffixOfSym2,
  IsInfixOfSym0, IsInfixOfSym1, IsInfixOfSym2,

  ElemSym0, ElemSym1, ElemSym2,
  NotElemSym0, NotElemSym1, NotElemSym2,

  ZipSym0, ZipSym1, ZipSym2,
  Zip3Sym0, Zip3Sym1, Zip3Sym2, Zip3Sym3,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3, ZipWith3Sym4,
  UnzipSym0, UnzipSym1,
  Unzip3Sym0, Unzip3Sym1,
  Unzip4Sym0, Unzip4Sym1,
  Unzip5Sym0, Unzip5Sym1,
  Unzip6Sym0, Unzip6Sym1,
  Unzip7Sym0, Unzip7Sym1,

  DeleteSym0, DeleteSym1, DeleteSym2,
  (:\\$), (:\\$$), (:\\$$$),
  IntersectSym0, IntersectSym1, IntersectSym2,

  InsertSym0, InsertSym1, InsertSym2,
  SortSym0, SortSym1,

  DeleteBySym0, DeleteBySym1, DeleteBySym2, DeleteBySym3,
  DeleteFirstsBySym0, DeleteFirstsBySym1, DeleteFirstsBySym2, DeleteFirstsBySym3,
  IntersectBySym0, IntersectBySym1, IntersectBySym2,

  SortBySym0, SortBySym1, SortBySym2,
  InsertBySym0, InsertBySym1, InsertBySym2, InsertBySym3,
  MaximumBySym0, MaximumBySym1, MaximumBySym2,
  MinimumBySym0, MinimumBySym1, MinimumBySym2,
  LengthSym0, LengthSym1,
  SumSym0, SumSym1, ProductSym0, ProductSym1,
  ReplicateSym0, ReplicateSym1, ReplicateSym2,
  TransposeSym0, TransposeSym1,
  TakeSym0, TakeSym1, TakeSym2,
  DropSym0, DropSym1, DropSym2,
  SplitAtSym0, SplitAtSym1, SplitAtSym2,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  DropWhileEndSym0, DropWhileEndSym1, DropWhileEndSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  StripPrefixSym0, StripPrefixSym1, StripPrefixSym2,
  MaximumSym0, MaximumSym1,
  MinimumSym0, MinimumSym1,
  GroupSym0, GroupSym1,
  GroupBySym0, GroupBySym1, GroupBySym2,
  LookupSym0, LookupSym1, LookupSym2,
  FindSym0, FindSym1, FindSym2,
  FilterSym0, FilterSym1, FilterSym2,
  PartitionSym0, PartitionSym1, PartitionSym2,

  (:!!$), (:!!$$), (:!!$$$),

  ElemIndexSym0, ElemIndexSym1, ElemIndexSym2,
  ElemIndicesSym0, ElemIndicesSym1, ElemIndicesSym2,
  FindIndexSym0, FindIndexSym1, FindIndexSym2,
  FindIndicesSym0, FindIndicesSym1, FindIndicesSym2,

  Zip4Sym0, Zip4Sym1, Zip4Sym2, Zip4Sym3, Zip4Sym4,
  Zip5Sym0, Zip5Sym1, Zip5Sym2, Zip5Sym3, Zip5Sym4, Zip5Sym5,
  Zip6Sym0, Zip6Sym1, Zip6Sym2, Zip6Sym3, Zip6Sym4, Zip6Sym5, Zip6Sym6,
  Zip7Sym0, Zip7Sym1, Zip7Sym2, Zip7Sym3, Zip7Sym4, Zip7Sym5, Zip7Sym6, Zip7Sym7,

  ZipWith4Sym0, ZipWith4Sym1, ZipWith4Sym2, ZipWith4Sym3, ZipWith4Sym4, ZipWith4Sym5,
  ZipWith5Sym0, ZipWith5Sym1, ZipWith5Sym2, ZipWith5Sym3, ZipWith5Sym4, ZipWith5Sym5, ZipWith5Sym6,
  ZipWith6Sym0, ZipWith6Sym1, ZipWith6Sym2, ZipWith6Sym3, ZipWith6Sym4, ZipWith6Sym5, ZipWith6Sym6, ZipWith6Sym7,
  ZipWith7Sym0, ZipWith7Sym1, ZipWith7Sym2, ZipWith7Sym3, ZipWith7Sym4, ZipWith7Sym5, ZipWith7Sym6, ZipWith7Sym7, ZipWith7Sym8,

  NubSym0, NubSym1,
  NubBySym0, NubBySym1, NubBySym2,
  UnionSym0, UnionSym1, UnionSym2,
  UnionBySym0, UnionBySym1, UnionBySym2, UnionBySym3,

  GenericLengthSym0, GenericLengthSym1,
  GenericTakeSym0, GenericTakeSym1, GenericTakeSym2,
  GenericDropSym0, GenericDropSym1, GenericDropSym2,
  GenericSplitAtSym0, GenericSplitAtSym1, GenericSplitAtSym2,
  GenericIndexSym0, GenericIndexSym1, GenericIndexSym2,
  GenericReplicateSym0, GenericReplicateSym1, GenericReplicateSym2,

  ) where

import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.TH

$(promoteOnly [d|

  -- Overlapping patterns don't singletonize
  stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
  stripPrefix [] ys = Just ys
  stripPrefix (x:xs) (y:ys)
   | x == y = stripPrefix xs ys
  stripPrefix _ _ = Nothing

  -- To singletonize these we would need to rewrite all patterns
  -- as non-overlapping. This means 2^7 equations for zipWith7.

  zip4                    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
  zip4                    =  zipWith4 (,,,)

  zip5                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
  zip5                    =  zipWith5 (,,,,)

  zip6                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [(a,b,c,d,e,f)]
  zip6                    =  zipWith6 (,,,,,)

  zip7                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [g] -> [(a,b,c,d,e,f,g)]
  zip7                    =  zipWith7 (,,,,,,)

  zipWith4                :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
  zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                          =  z a b c d : zipWith4 z as bs cs ds
  zipWith4 _ _ _ _ _      =  []

  zipWith5                :: (a->b->c->d->e->f) ->
                             [a]->[b]->[c]->[d]->[e]->[f]
  zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                          =  z a b c d e : zipWith5 z as bs cs ds es
  zipWith5 _ _ _ _ _ _    = []

  zipWith6                :: (a->b->c->d->e->f->g) ->
                             [a]->[b]->[c]->[d]->[e]->[f]->[g]
  zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                          =  z a b c d e f : zipWith6 z as bs cs ds es fs
  zipWith6 _ _ _ _ _ _ _  = []

  zipWith7                :: (a->b->c->d->e->f->g->h) ->
                             [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
  zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                     =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
  zipWith7 _ _ _ _ _ _ _ _ = []

-- These functions use Integral or Num typeclass instead of Int.
--
--  genericLength, genericTake, genericDrop, genericSplitAt, genericIndex
--  genericReplicate
--
-- We provide aliases below to improve compatibility

  genericTake :: (Integral i) => i -> [a] -> [a]
  genericTake = take

  genericDrop :: (Integral i) => i -> [a] -> [a]
  genericDrop = drop

  genericSplitAt :: (Integral i) => i -> [a] -> ([a], [a])
  genericSplitAt = splitAt

  genericIndex :: (Integral i) => [a] -> i -> a
  genericIndex = (!!)

  genericReplicate :: (Integral i) => i -> a -> [a]
  genericReplicate = replicate
 |])
