{-# LANGUAGE CPP, TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.List
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Re-export promoted definitions related to list datatype.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.List (
  -- Transpose,
  TakeWhile, DropWhile, DropWhileEnd, Span, Break,
  StripPrefix,

  Maximum, Minimum,

  -- * Sublists

  -- ** Extracting sublists
  Group,

  -- * Searching lists

  -- ** Searching by equality
  Lookup,

  -- ** Searching with a predicate
  Find, Filter, Partition,

  -- * Zipping and unzipping lists
  Zip4, Zip5, Zip6, Zip7,
  ZipWith4, ZipWith5, ZipWith6, ZipWith7,

  -- * Special lists

  -- ** \"Set\" operations
  Nub, Union,

  -- * Generalized functions

  -- ** The \"@By@\" operations
  GroupBy, NubBy, UnionBy,

  -- * Defunctionalization symbols
--  TransposeSym0, TransposeSym1,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  DropWhileEndSym0, DropWhileEndSym1, DropWhileEndSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  StripPrefixSym0, StripPrefixSym1,
  MaximumSym0, MaximumSym1,
  MinimumSym0, MinimumSym1,
  GroupSym0, GroupSym1,
  GroupBySym0, GroupBySym1, GroupBySym2,
  LookupSym0, LookupSym1, LookupSym2,
  FindSym0, FindSym1, FindSym2,
  FilterSym0, FilterSym1, FilterSym2,
  PartitionSym0, PartitionSym1, PartitionSym2,

  Zip4Sym0, Zip4Sym1, Zip4Sym2, Zip4Sym3, Zip4Sym4,
  Zip5Sym0, Zip5Sym1, Zip5Sym2, Zip5Sym3, Zip5Sym4, Zip5Sym5,
  Zip6Sym0, Zip6Sym1, Zip6Sym2, Zip6Sym3, Zip6Sym4, Zip6Sym5, Zip6Sym6,
  Zip7Sym0, Zip7Sym1, Zip7Sym2, Zip7Sym3, Zip7Sym4, Zip7Sym5, Zip7Sym6, Zip7Sym7,

  ZipWith4Sym0, ZipWith4Sym1, ZipWith4Sym2, ZipWith4Sym3, ZipWith4Sym4,
  ZipWith5Sym0, ZipWith5Sym1, ZipWith5Sym2, ZipWith5Sym3, ZipWith5Sym4, ZipWith5Sym5,
  ZipWith6Sym0, ZipWith6Sym1, ZipWith6Sym2, ZipWith6Sym3, ZipWith6Sym4, ZipWith6Sym5, ZipWith6Sym6,
  ZipWith7Sym0, ZipWith7Sym1, ZipWith7Sym2, ZipWith7Sym3, ZipWith7Sym4, ZipWith7Sym5, ZipWith7Sym6, ZipWith7Sym7,

  NubSym0, NubSym1,
  NubBySym0, NubBySym1, NubBySym2,
  UnionSym0, UnionSym1, UnionSym2,
  UnionBySym0, UnionBySym1, UnionBySym2, UnionBySym3,

  -- * Re-exports from Data.Singletons.Prelude
  (:++), Head, Last, Tail, Init, Null,

   -- * List transformations
  Map, Reverse, Intersperse, Intercalate, Subsequences, Permutations,

  -- * Reducing lists (folds)
  Foldl, Foldl', Foldl1, Foldl1', Foldr, Foldr1,

  -- ** Special folds
  Concat, ConcatMap, And, Or, Any_, All,
  any_, -- equivalent of Data.List `any`. Avoids name clash with Any type

  -- * Building lists

  -- ** Scans
  Scanl, Scanl1, Scanr, Scanr1,

  -- ** Accumulating maps
  MapAccumL, MapAccumR,

  -- ** Unfolding
  Unfoldr,

  -- * Sublists

  -- ** Extracting sublists
  Inits, Tails,

  -- ** Predicates
  IsPrefixOf, IsSuffixOf, IsInfixOf,

  -- * Searching lists

  -- ** Searching by equality
  Elem, NotElem,

  -- * Zipping and unzipping lists
  Zip, Zip3, ZipWith, ZipWith3,
  Unzip, Unzip3, Unzip4, Unzip5, Unzip6, Unzip7,

  -- * Special lists

  -- ** \"Set\" operations
  Delete, (:\\), --Intersect,

  -- ** Ordered lists
  -- Insert, Sort,

  -- * Generalized functions

  -- ** The \"@By@\" operations
  DeleteBy, DeleteFirstsBy,
  -- IntersectBy,

  SortBy, InsertBy,
  MaximumBy, MinimumBy,

  -- * Defunctionalization symbols
  (:$), (:$$), (:$$$),
  NilSym0, ConsSym0, ConsSym1, ConsSym2,

  (:++$$), (:++$), HeadSym0, HeadSym1, LastSym0, LastSym1,
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
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3,
  UnzipSym0, UnzipSym1,
  Unzip3Sym0, Unzip3Sym1,
  Unzip4Sym0, Unzip4Sym1,
  Unzip5Sym0, Unzip5Sym1,
  Unzip6Sym0, Unzip6Sym1,
  Unzip7Sym0, Unzip7Sym1,

  DeleteSym0, DeleteSym1, DeleteSym2,
  (:\\$), (:\\$$), (:\\$$$),
  -- IntersectSym0, IntersectSym1, IntersectSym2,

  InsertSym0, InsertSym1, InsertSym2,
  SortSym0, SortSym1,

  DeleteBySym0, DeleteBySym1, DeleteBySym2, DeleteBySym3,
  DeleteFirstsBySym0, DeleteFirstsBySym1, DeleteFirstsBySym2, DeleteFirstsBySym3,
  -- IntersectBySym0, IntersectBySym1, IntersectBySym2,

  SortBySym0, SortBySym1, SortBySym2,
  InsertBySym0, InsertBySym1, InsertBySym2, InsertBySym3,
  MaximumBySym0, MaximumBySym1, MaximumBySym2,
  MinimumBySym0, MinimumBySym1, MinimumBySym2,
  ) where

import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Eq
import Data.Promotion.Prelude.Ord
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.TH

import Data.Maybe (listToMaybe)
-- these imports are required fir functions that singletonize but are used
-- in this module by a function that can't be singletonized
import Data.List  (deleteBy, sortBy, insertBy)

$(promoteOnly [d|
-- Needs monad promotion
--  transpose               :: [[a]] -> [[a]]
--  transpose []             = []
--  transpose ([]   : xss)   = transpose xss
--  transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

  takeWhile               :: (a -> Bool) -> [a] -> [a]
  takeWhile _ []          =  []
  takeWhile p (x:xs)
              | p x       =  x : takeWhile p xs
              | otherwise =  []

  dropWhile               :: (a -> Bool) -> [a] -> [a]
  dropWhile _ []          =  []
  dropWhile p xs@(x:xs')
              | p x       =  dropWhile p xs'
              | otherwise =  xs

  dropWhileEnd            :: (a -> Bool) -> [a] -> [a]
  dropWhileEnd p          = foldr (\x xs -> if p x && null xs then [] else x : xs) []

  span                    :: (a -> Bool) -> [a] -> ([a],[a])
  span _ xs@[]            =  (xs, xs)
  span p xs@(x:xs')
           | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
           | otherwise    =  ([],xs)

  break                   :: (a -> Bool) -> [a] -> ([a],[a])
  break _ xs@[]           =  (xs, xs)
  break p xs@(x:xs')
             | p x        =  ([],xs)
             | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)

  -- Overlapping patterns don't singletonize
  stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
  stripPrefix [] ys = Just ys
  stripPrefix (x:xs) (y:ys)
   | x == y = stripPrefix xs ys
  stripPrefix _ _ = Nothing

  -- Relies on groupBy, which relies on span, which does not singletonize
  group                   :: Eq a => [a] -> [[a]]
  group xs                =  groupBy (==) xs

  -- Requires Ord instance, which does not singletonize
  maximum                 :: (Ord a) => [a] -> a
  maximum []              =  error "Data.Singletons.List.maximum: empty list"
  maximum xs              =  foldl1 max xs

  -- Requires Ord instance, which does not singletonize
  minimum                 :: (Ord a) => [a] -> a
  minimum []              =  error "Data.Singletons.List.minimum: empty list"
  minimum xs              =  foldl1 min xs

  -- Requires Ord instance, which does not singletonize
  insert :: Ord a => a -> [a] -> [a]
  insert e ls = insertBy (compare) e ls

  -- Requires Ord instance, which does not singletonize
  sort :: (Ord a) => [a] -> [a]
  -- Temporalily eta-expanded to work around #31
  sort xs = sortBy compare xs

  -- Relies on span, which does not singletonize
  groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
  groupBy _  []           =  []
  groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                             where (ys,zs) = span (eq x) xs

  lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
  lookup _key []          =  Nothing
  lookup  key ((x,y):xys)
      | key == x          =  Just y
      | otherwise         =  lookup key xys

  -- Relies on filter, which does not singletonize
  find                    :: (a -> Bool) -> [a] -> Maybe a
  find p                  = listToMaybe . filter p

  filter :: (a -> Bool) -> [a] -> [a]
  filter _p []            = []
  filter p (x:xs)
    | p x                 = x : filter p xs
    | otherwise           = filter p xs

  -- Relies on select, which does not singletonize (#30, #33)
  partition               :: (a -> Bool) -> [a] -> ([a],[a])
  partition p xs          = foldr (select p) ([],[]) xs

  -- Lazy pattern removed from select
  select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
  select p x (ts,fs) | p x       = (x:ts,fs)
                     | otherwise = (ts, x:fs)

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

  nub                     :: (Eq a) => [a] -> [a]
  nub l                   = nub' l []
    where
      nub' :: [a] -> [a] -> [a]
      nub' [] _           = []
      nub' (x:xs) ls
          | x `elem` ls   = nub' xs ls
          | otherwise     = x : nub' xs (x:ls)

  nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
  nubBy eq l              = nubBy' l []
    where
      nubBy' :: [b] -> [b] -> [b]
      nubBy' [] _         = []
      nubBy' (y:ys) xs
         | elem_by eq y xs = nubBy' ys xs
         | otherwise       = y : nubBy' ys (y:xs)

  elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
  elem_by _  _ []         =  False
  elem_by eq y (x:xs)     =  y `eq` x || elem_by eq y xs

  -- This relies on nubBy, which does not singletonize
  unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

  -- This relies on unionBy, which does not singletonize
  union                   :: (Eq a) => [a] -> [a] -> [a]
  union                   = unionBy (==)
 |])
