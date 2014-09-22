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
  (:$), (:$$), (:$$$),
  NilSym0, ConsSym0, ConsSym1, ConsSym2,

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
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Eq
import Data.Promotion.Prelude.Ord
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Tuple
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num

import Data.Maybe (listToMaybe)
-- these imports are required fir functions that singletonize but are used
-- in this module by a function that can't be singletonized
import Data.List  (deleteBy, sortBy, insertBy)

$(promoteOnly [d|
-- Can't be promoted because of limitations of Int promotion
-- Below is a re-implementation using Nat
--  length                  :: [a] -> Int
--  length l                =  lenAcc l 0#
--
--  lenAcc :: [a] -> Int# -> Int
--  lenAcc []     a# = I# a#
--  lenAcc (_:xs) a# = lenAcc xs (a# +# 1#)
--
--  incLen :: a -> (Int# -> Int) -> Int# -> Int
--  incLen _ g x = g (x +# 1#)

  length :: [a] -> Nat
  length []     = 0
  length (_:xs) = 1 + length xs

-- Can't be promoted because of limitations of Int promotion
-- Below is a re-implementation using Nat
--  sum                     :: (Num a) => [a] -> a
--  sum     l       = sum' l 0
--    where
--      sum' []     a = a
--      sum' (x:xs) a = sum' xs (a+x)
--
--  product                 :: (Num a) => [a] -> a
--  product l       = prod l 1
--    where
--      prod []     a = a
--      prod (x:xs) a = prod xs (a*x)

  sum                     :: [Nat] -> Nat
  sum     l       = sum' l 0
    where
      sum' []     a = a
      sum' (x:xs) a = sum' xs (a+x)

  product                 :: [Nat] -> Nat
  product l       = prod l 1
    where
      prod []     a = a
      prod (x:xs) a = prod xs (a*x)

-- Functions working on infinite lists don't promote because they create
-- infinite types. replicate also uses integers, but luckily it can be rewritten
--  iterate :: (a -> a) -> a -> [a]
--  iterate f x =  x : iterate f (f x)
--
--  repeat :: a -> [a]
--  repeat x = xs where xs = x : xs
--
--  replicate               :: Int -> a -> [a]
--  replicate n x           =  take n (repeat x)
--
--  cycle                   :: [a] -> [a]
--  cycle []                = error "Data.Singletons.List.cycle: empty list"
--  cycle xs                = xs' where xs' = xs ++ xs'

  replicate               :: Nat -> a -> [a]
  replicate 0 _           = []
  replicate n x           = x : replicate (n-1) x

-- Uses list comprehensions
--  transpose               :: [[a]] -> [[a]]
--  transpose []             = []
--  transpose ([]   : xss)   = transpose xss
--  transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

  transpose               :: [[a]] -> [[a]]
  transpose []             = []
  transpose ([]   : xss)   = transpose xss
  transpose ((x:xs) : xss) = (x : (map head xss)) : transpose (xs : (map tail xss))

-- Can't be promoted because of limitations of Int promotion
-- Below is a re-implementation using Nat
--  take                   :: Int -> [a] -> [a]
--  take n _      | n <= 0 =  []
--  take _ []              =  []
--  take n (x:xs)          =  x : take (n-1) xs

--  drop                   :: Int -> [a] -> [a]
--  drop n xs     | n <= 0 =  xs
--  drop _ []              =  []
--  drop n (_:xs)          =  drop (n-1) xs

--  splitAt                :: Int -> [a] -> ([a],[a])
--  splitAt n xs           =  (take n xs, drop n xs)

  -- Implementation changed to use case expression to work around #60
  take                   :: Nat -> [a] -> [a]
  take _ []              =  []
  take 0 (_:_)           =  []
  take n (x:xs)          =  x : take (n-1) xs

  drop                   :: Nat -> [a] -> [a]
  drop _ []              =  []
  drop 0 xs@(_:_)        =  xs
  drop n (_:xs)          =  drop (n-1) xs

  splitAt                :: Nat -> [a] -> ([a],[a])
  splitAt n xs           =  (take n xs, drop n xs)

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
  sort = sortBy compare

-- Can't be promoted because of limitations of Int promotion.
-- Also, #60 and th-desugar #6 get in the way.
-- Below is a re-implementation using Nat
--  (!!)                    :: [a] -> Int -> a
--  xs     !! n | n < 0 =  error "Data.Singletons.List.!!: negative index"
--  []     !! _         =  error "Data.Singletons.List.!!: index too large"
--  (x:_)  !! 0         =  x
--  (_:xs) !! n         =  xs !! (n-1)

  (!!)                    :: [a] -> Nat -> a
  []     !! _ = error "Data.Singletons.List.!!: index too large"
  (x:xs) !! n = if | n == 0    -> x
                   | otherwise -> xs !! (n-1)

-- These three rely on findIndices, which does not promote.
-- Since we have our own implementation of findIndices these are perfectly valid
  elemIndex       :: Eq a => a -> [a] -> Maybe Nat
  elemIndex x     = findIndex (x==)

  elemIndices     :: Eq a => a -> [a] -> [Nat]
  elemIndices x   = findIndices (x==)

  findIndex       :: (a -> Bool) -> [a] -> Maybe Nat
  findIndex p     = listToMaybe . findIndices p

-- Uses list comprehensions, infinite lists and and Ints
--  findIndices      :: (a -> Bool) -> [a] -> [Int]
--  findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]

  findIndices      :: (a -> Bool) -> [a] -> [Nat]
  findIndices p xs = map snd (filter (\(x,_) -> p x)
                                     (zip xs (buildList 0 (length xs))))
    where buildList _ 0 = []
          buildList a n = a : buildList (a+1) (n-1)

-- These functions use Integral or Num typeclass instead of Int.
--
--  genericLength, genericTake, genericDrop, genericSplitAt, genericIndex
--  genericReplicate
--
-- We provide aliases below to improve compatibility

  genericLength :: (Num i) => [a] -> i
  genericLength = length

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
