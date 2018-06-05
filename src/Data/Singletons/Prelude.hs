-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mimics the Haskell Prelude, but with singleton types. Includes the basic
-- singleton definitions. Note: This is currently very incomplete!
--
-- Because many of these definitions are produced by Template Haskell, it is
-- not possible to create proper Haddock documentation. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction between
-- Template Haskell and Haddock.
--
----------------------------------------------------------------------------

{-# LANGUAGE ExplicitNamespaces #-}
module Data.Singletons.Prelude (
  -- * Basic singleton definitions
  module Data.Singletons,

  Sing(SFalse, STrue, SNil, SCons, SJust, SNothing, SLeft, SRight, SLT, SEQ, SGT,
       STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7),

  -- * Singleton type synonyms

  -- | These synonyms are all kind-restricted synonyms of 'Sing'.
  -- For example 'SBool' requires an argument of kind 'Bool'.
  SBool, SList, SMaybe, SEither, SOrdering,
  STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,

  -- * Functions working with 'Bool'
  If, sIf, Not, sNot, type (&&), type (||), (%&&), (%||), Otherwise, sOtherwise,

  -- * Error reporting
  Error, sError,
  ErrorWithoutStackTrace, sErrorWithoutStackTrace,
  Undefined, sUndefined,

  -- * Singleton equality
  module Data.Singletons.Prelude.Eq,

  -- * Singleton comparisons
  POrd(..), SOrd(..),

  -- * Singleton Enum and Bounded
  -- | As a matter of convenience, the singletons Prelude does /not/ export
  -- promoted/singletonized @succ@ and @pred@, due to likely conflicts with
  -- unary numbers. Please import 'Data.Singletons.Prelude.Enum' directly if
  -- you want these.
  module Data.Singletons.Prelude.Enum,

  -- * Singletons numbers
  module Data.Singletons.Prelude.Num,
  type (^), (%^),

  -- * Singleton 'Show'
  PShow(..), SShow(..), ShowS, SChar,
  Shows, sShows, ShowChar, sShowChar, ShowString, sShowString, ShowParen, sShowParen,

  -- * Singleton 'Semigroup' and 'Monoid'
  PSemigroup(type (<>)), SSemigroup((%<>)),
  PMonoid(..), SMonoid(..),

  -- * Singleton 'Functor', 'Applicative', and 'Monad'
  PFunctor(Fmap, type (<$)), SFunctor(sFmap, (%<$)), type (<$>), (%<$>),
  PApplicative(Pure, type (<*>), type (*>), type (<*)),
  SApplicative(sPure, (%<*>), (%*>), (%<*)),
  PMonad(type (>>=), type (>>), Return, Fail),
  SMonad((%>>=), (%>>), sReturn, sFail),
  -- MapM_, sMapM_,
  type (=<<), (%=<<),

  -- ** Miscellaneous functions
  Id, sId, Const, sConst, (:.), (%.), type ($), (%$), type ($!), (%$!),
  Flip, sFlip, AsTypeOf, sAsTypeOf,
  Seq, sSeq,

  -- * List operations
  Map, sMap, type (++), (%++), Head, sHead, Last, sLast, Tail, sTail,
  Init, sInit, Null, sNull, Reverse, sReverse,
  -- ** Reducing lists (folds)
  Foldl, sFoldl, Foldl1, sFoldl1, Foldr, sFoldr, Foldr1, sFoldr1,
  -- *** Special folds
  And, sAnd, Or, sOr, Any, sAny, All, sAll,
  Concat, sConcat, ConcatMap, sConcatMap,
  -- *** Scans
  Scanl, sScanl, Scanl1, sScanl1, Scanr, sScanr, Scanr1, sScanr1,
  -- *** Infinite lists
  Replicate, sReplicate,
  -- ** Sublists
  Take, sTake, Drop, sDrop, SplitAt, sSplitAt, TakeWhile, sTakeWhile,
  Span, sSpan, Break, sBreak,
  -- ** Searching lists
  Elem, sElem, NotElem, sNotElem, Lookup, sLookup,
  -- ** Zipping and unzipping lists
  Zip, sZip, Zip3, sZip3, ZipWith, sZipWith, ZipWith3, sZipWith3,
  Unzip, sUnzip, Unzip3, sUnzip3,
  -- ** Functions on 'Symbol's
  Unlines, sUnlines, Unwords, sUnwords,

  -- * Other datatypes
  Maybe_, sMaybe_,
  Either_, sEither_,
  Fst, sFst, Snd, sSnd, Curry, sCurry, Uncurry, sUncurry,
  Symbol,

  -- * Other functions
  either_, -- reimplementation of either to be used with singletons library
  maybe_,
  bool_,
  show_,

  -- * Defunctionalization symbols
  FalseSym0, TrueSym0,
  NotSym0, NotSym1,
  type (&&@#@$), type (&&@#@$$), type (&&@#@$$$),
  type (||@#@$), type (||@#@$$), type (||@#@$$$),
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

  ErrorSym0, ErrorSym1,
  ErrorWithoutStackTraceSym0, ErrorWithoutStackTraceSym1,
  UndefinedSym0,

  LTSym0, EQSym0, GTSym0,
  CompareSym0, CompareSym1, CompareSym2,
  type (<@#@$),  type (<@#@$$),  type (<@#@$$$),
  type (<=@#@$), type (<=@#@$$), type (<=@#@$$$),
  type (>@#@$),  type (>@#@$$),  type (>@#@$$$),
  type (>=@#@$), type (>=@#@$$), type (>=@#@$$$),
  MaxSym0, MaxSym1, MaxSym2,
  MinSym0, MinSym1, MinSym2,

  type (^@#@$), type (^@#@$$), type (^@#@$$$),

  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  Show_Sym0, Show_Sym1,
  ShowListSym0, ShowListSym1, ShowListSym2,
  ShowsSym0, ShowsSym1, ShowsSym2,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,

  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MconcatSym0, MconcatSym1,

  FmapSym0, FmapSym1, FmapSym2,
  type (<$@#@$),  type (<$@#@$$),  type (<$@#@$$$),
  type (<$>@#@$), type (<$>@#@$$), type (<$>@#@$$$),
  PureSym0, PureSym1,
  type (<*>@#@$), type (<*>@#@$$), type (<*>@#@$$$),
  type (*>@#@$),  type (*>@#@$$),  type (*>@#@$$$),
  type (<*@#@$),  type (<*@#@$$),  type (<*@#@$$$),
  type (>>=@#@$), type (>>=@#@$$), type (>>=@#@$$$),
  type (>>@#@$),  type (>>@#@$$),  type (>>@#@$$$),
  ReturnSym0, ReturnSym1, FailSym0, FailSym1,
  type (=<<@#@$), type (=<<@#@$$), type (=<<@#@$$$),

  IdSym0, IdSym1, ConstSym0, ConstSym1, ConstSym2,
  type (.@#@$),  type (.@#@$$),  type (.@#@$$$),
  type ($@#@$),  type ($@#@$$),  type ($@#@$$$),
  type ($!@#@$), type ($!@#@$$), type ($!@#@$$$),
  FlipSym0, FlipSym1, FlipSym2,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2, SeqSym0, SeqSym1, SeqSym2,

  (:@#@$), (:@#@$$), (:@#@$$$), NilSym0,
  MapSym0, MapSym1, MapSym2, ReverseSym0, ReverseSym1,
  type (++@#@$$), type (++@#@$), HeadSym0, HeadSym1, LastSym0, LastSym1,
  TailSym0, TailSym1, InitSym0, InitSym1, NullSym0, NullSym1,

  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  Foldr1Sym0, Foldr1Sym1, Foldr1Sym2,

  ConcatSym0, ConcatSym1,
  ConcatMapSym0, ConcatMapSym1, ConcatMapSym2,
  AndSym0, AndSym1, OrSym0, OrSym1,
  AnySym0, AnySym1, AnySym2,
  AllSym0, AllSym1, AllSym2,

  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,

  ReplicateSym0, ReplicateSym1, ReplicateSym2,

  TakeSym0, TakeSym1, TakeSym2,
  DropSym0, DropSym1, DropSym2,
  SplitAtSym0, SplitAtSym1, SplitAtSym2,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  DropWhileEndSym0, DropWhileEndSym1, DropWhileEndSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,

  ElemSym0, ElemSym1, ElemSym2,
  NotElemSym0, NotElemSym1, NotElemSym2,

  ZipSym0, ZipSym1, ZipSym2,
  Zip3Sym0, Zip3Sym1, Zip3Sym2, Zip3Sym3,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3,
  UnzipSym0, UnzipSym1,

  UnlinesSym0, UnlinesSym1, UnwordsSym0, UnwordsSym1
  ) where

import Data.Singletons
import Data.Singletons.Prelude.Applicative
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.Functor
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Monad
import Data.Singletons.Prelude.Tuple
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Enum
  hiding (Succ, Pred, SuccSym0, SuccSym1, PredSym0, PredSym1, sSucc, sPred)
import Data.Singletons.Prelude.Monoid
       ( PMonoid(..), SMonoid(..), MemptySym0, MappendSym0
       , MappendSym1, MappendSym2, MconcatSym0, MconcatSym1)
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Semigroup
       ( PSemigroup(..), SSemigroup(..)
       , type (<>@#@$), type (<>@#@$$), type (<>@#@$$$) )
import Data.Singletons.Prelude.Show
import Data.Singletons.TypeLits
