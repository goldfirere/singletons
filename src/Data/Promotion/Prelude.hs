-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mimics the Haskell Prelude, but with promoted types.
--
----------------------------------------------------------------------------

{-# LANGUAGE ExplicitNamespaces #-}
module Data.Promotion.Prelude (
  -- * Base
  Until,

  -- ** Sublists
  TakeWhile, DropWhile, Span, Break,
  -- ** Searching lists
  Lookup, Filter,

  -- * Defunctionalization symbols
  UntilSym0, UntilSym1, UntilSym2, UntilSym3,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  LookupSym0, LookupSym1, LookupSym2,
  FilterSym0, FilterSym1, FilterSym2,

  -- * Re-exports from Data.Singletons.Prelude

  -- * Functions working with 'Bool'
  If, Not, (:&&), (:||), Otherwise,

  -- * Error reporting
  Error, ErrorSym0,

  -- * Singleton equality
  type (==), (:==), (:/=),

  -- ** Miscellaneous functions
  Id, Const, (:.), Flip, AsTypeOf, Seq,

  -- * List operations
  Map, (:++), Head, Last, Tail, Init, Null, Reverse,
  -- ** Reducing lists (folds)
  Foldl, Foldl1, Foldr, Foldr1,
  -- *** Special folds
  And, Or, Any_, All, Concat, ConcatMap,
  -- *** Scans
  Scanl, Scanl1, Scanr, Scanr1,
  -- ** Searching lists
  Elem, NotElem,
  -- ** Zipping and unzipping lists
  Zip, Zip3, ZipWith, ZipWith3, Unzip, Unzip3,

  -- * Other datatypes
  Maybe_, Either_, Symbol,
  Fst, Snd, Curry, Uncurry,

  -- * Other functions
  either_, -- reimplementation of either to be used with singletons library
  any_, -- equivalent of Data.List `any`. Avoids name clash with Any type

  -- * Defunctionalization symbols
  FalseSym0, TrueSym0,
  NotSym0, NotSym1, (:&&$), (:&&$$), (:&&$$$), (:||$), (:||$$), (:||$$$),
  OtherwiseSym0,

  NothingSym0, JustSym0, JustSym1,
  Maybe_Sym0, Maybe_Sym1, Maybe_Sym2, Maybe_Sym3,

  LeftSym0, LeftSym1, RightSym0, RightSym1,
  Either_Sym0, Either_Sym1, Either_Sym2, Either_Sym3,

  Tuple0Sym0,
  Tuple2Sym0, Tuple2Sym1, Tuple2Sym2,
  Tuple3Sym0, Tuple3Sym1, Tuple3Sym2, Tuple3Sym3,
  Tuple4Sym0, Tuple4Sym1, Tuple4Sym2, Tuple4Sym3, Tuple4Sym4,
  Tuple5Sym0, Tuple5Sym1, Tuple5Sym2, Tuple5Sym3, Tuple5Sym4, Tuple5Sym5,
  Tuple6Sym0, Tuple6Sym1, Tuple6Sym2, Tuple6Sym3, Tuple6Sym4, Tuple6Sym5, Tuple6Sym6,
  Tuple7Sym0, Tuple7Sym1, Tuple7Sym2, Tuple7Sym3, Tuple7Sym4, Tuple7Sym5, Tuple7Sym6, Tuple7Sym7,
  FstSym0, FstSym1, SndSym0, SndSym1,
  CurrySym0, CurrySym1, CurrySym2, CurrySym3,
  UncurrySym0, UncurrySym1, UncurrySym2,

  (:==$), (:==$$), (:==$$$), (:/=$), (:/=$$), (:/=$$$),

  EQSym0, LTSym0, GTSym0,

  IdSym0, IdSym1, ConstSym0, ConstSym1, ConstSym2,
  (:.$), (:.$$), (:.$$$), FlipSym0, FlipSym1, FlipSym2,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2, SeqSym0, SeqSym1, SeqSym2,

  (:$), (:$$), (:$$$), NilSym0, ConsSym0, ConsSym1, ConsSym2,
  MapSym0, MapSym1, MapSym2, ReverseSym0, ReverseSym1,
  (:++$$), (:++$), HeadSym0, HeadSym1, LastSym0, LastSym1,
  TailSym0, TailSym1, InitSym0, InitSym1, NullSym0, NullSym1,

  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
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

  ElemSym0, ElemSym1, ElemSym2,
  NotElemSym0, NotElemSym1, NotElemSym2,

  ZipSym0, ZipSym1, ZipSym2,
  Zip3Sym0, Zip3Sym1, Zip3Sym2, Zip3Sym3,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3,
  UnzipSym0, UnzipSym1
  ) where

import Data.Promotion.Prelude.Base
import Data.Promotion.Prelude.Bool
import Data.Promotion.Prelude.Either
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Maybe
import Data.Promotion.Prelude.Tuple
import Data.Promotion.Prelude.Eq
import Data.Singletons.Prelude.Instances (EQSym0, LTSym0, GTSym0)
import Data.Singletons.TypeLits (Error, ErrorSym0, Symbol)
