{-# LANGUAGE NoStarIsType #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
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

module Prelude.Singletons (
  -- * Basic singleton definitions
  module Data.Singletons,

  -- * Promoted and singled types, classes, and related functions

  -- ** Basic data types
  SBool(SFalse, STrue),
  If, sIf,
  type (&&), (%&&), type (||), (%||), Not, sNot, Otherwise, sOtherwise,

  SMaybe(SNothing, SJust),
  -- | 'maybe_' is a reimplementation of the 'maybe' function with a different
  -- name to avoid clashing with the 'Maybe' data type when promoted.
  maybe_, Maybe_, sMaybe_,

  SEither(SLeft, SRight),
  -- | 'either_' is a reimplementation of the 'either' function with a different
  -- name to avoid clashing with the 'Either' data type when promoted.
  either_, Either_, sEither_,

  SOrdering(SLT, SEQ, SGT),
  SChar, Symbol, -- The closest things we have to Char and String
  SList(..),

  -- *** Tuples
  STuple0(..), STuple2(..), STuple3(..), STuple4(..),
  STuple5(..), STuple6(..), STuple7(..),
  Fst, sFst, Snd, sSnd, Curry, sCurry, Uncurry, sUncurry,

  -- ** Basic type classes
  PEq(type (==), type (/=)), SEq((%==), (%/=)),
  POrd(Compare, type (<), type (<=), type (>=), type (>), Max, Min),
  SOrd(sCompare, (%<), (%<=), (%>=), (%>), sMax, sMin),
  -- | As a matter of convenience, the "Prelude.Singletons" does /not/ export
  -- promoted/singletonized @succ@ and @pred@, due to likely conflicts with
  -- unary numbers. Please import "Data.Singletons.Base.Enum" directly if
  -- you want these.
  PEnum( -- Succ, Pred,
         ToEnum, FromEnum,
         {-
         See the comments in Data.Singletons.Base.Enum for why these are not defined.
         -}
         -- EnumFrom, EnumFromThen,
         EnumFromTo, EnumFromThenTo),
  SEnum( -- sSucc, sPred,
         sToEnum, sFromEnum,
         {-
         See the comments in Data.Singletons.Base.Enum for why these are not defined.
         -}
         -- sEnumFrom, sEnumFromThen,
         sEnumFromTo, sEnumFromThenTo),
  PBounded(MinBound, MaxBound), SBounded(sMinBound, sMaxBound),

  -- ** Numbers

  {-
  There are no singled counterparts to any of the following numeric types. The
  closest thing is Nat, which we deliberately do not export to avoid clashing
  with unary natural number data types.

  -- *** Numeric types
  Int, Integer, Float, Double,
  Rational, Word,
  -}

  -- *** Numeric type classes
  PNum(type (+), type (-), type (*), Negate, Abs, Signum, FromInteger),
  SNum((%+), (%-), (%*), sNegate, sAbs, sSignum, sFromInteger),
  {-
  We currently do not promote or single any of the following classes. Some
  of these functions have Nat-specialized versions in GHC.TypeLits.Singletons,
  however (e.g., Div and Mod).
  -}
  -- Real(toRational),
  -- Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
  -- Fractional((/), recip, fromRational),
  -- Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
  --          asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
  -- RealFrac(properFraction, truncate, round, ceiling, floor),
  -- RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
  --           encodeFloat, exponent, significand, scaleFloat, isNaN,
  --           isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

  -- *** Numeric functions
  Subtract, sSubtract,
  {-
  The following functions require classes such as Integral, Real, and
  Fractional, which we do not promote or single. Some of these functions have
  Nat-specialized versions in GHC.TypeLits.Singletons, however (e.g., (^)).
  -}
  -- even, odd, gcd, lcm, (^), (^^),
  -- fromIntegral, realToFrac,

  -- ** Semigroups and Monoids
  PSemigroup(type (<>)), SSemigroup((%<>)),
  PMonoid(Mempty, Mappend, Mconcat), SMonoid(sMempty, sMappend, sMconcat),

  -- ** Monads and functors
  PFunctor(Fmap, type (<$)), SFunctor(sFmap, (%<$)), type (<$>), (%<$>),
  PApplicative(Pure, type (<*>), type (*>), type (<*), LiftA2),
  SApplicative(sPure, (%<*>), (%*>), (%<*), sLiftA2),
  PMonad(type (>>=), type (>>), Return),
  SMonad((%>>=), (%>>), sReturn),
  PMonadFail(Fail), SMonadFail(sFail),
  MapM_, sMapM_, Sequence_, sSequence_, type (=<<), (%=<<),

  -- ** Folds and traversals
  PFoldable(Elem, FoldMap, Foldr, Foldl, Foldr1, Foldl1,
            Maximum, Minimum, Product, Sum),
  SFoldable(sElem, sFoldMap, sFoldr, sFoldl, sFoldr1, sFoldl1,
            sMaximum, sMinimum, sProduct, sSum),
  PTraversable(Traverse, SequenceA, MapM, Sequence),
  STraversable(sTraverse, sSequenceA, sMapM, sSequence),

  -- ** Miscellaneous functions
  Id, sId, Const, sConst, type (.), (%.), Flip, sFlip, type ($), (%$), Until, sUntil,
  AsTypeOf, sAsTypeOf,
  Error, sError, ErrorWithoutStackTrace, sErrorWithoutStackTrace,
  Undefined, sUndefined,
  Seq, sSeq, type ($!), (%$!),

  -- * List operations
  Map, sMap, type (++), (%++), Filter, sFilter,
  Head, sHead, Last, sLast, Tail, sTail, Init, sInit, type (!!), (%!!),
  Null, sNull, Length, sLength,
  Reverse, sReverse,
  -- *** Special folds
  And, sAnd, Or, sOr, Any, sAny, All, sAll,
  Concat, sConcat, ConcatMap, sConcatMap,
  -- ** Building lists
  -- *** Scans
  Scanl, sScanl, Scanl1, sScanl1, Scanr, sScanr, Scanr1, sScanr1,
  -- *** Infinite lists
  {-
  Unsurprisingly, most operations on infinite lists won't promote.
  The `replicate` function is the lone exception.
  -}
  -- iterate, repeat,
  Replicate, sReplicate,
  -- cycle,
  -- ** Sublists
  Take, sTake, Drop, sDrop,
  TakeWhile, sTakeWhile, DropWhile, sDropWhile,
  Span, sSpan, Break, sBreak,
  SplitAt, sSplitAt,
  -- ** Searching lists
  NotElem, sNotElem,
  Lookup, sLookup,
  -- ** Zipping and unzipping lists
  Zip, sZip, Zip3, sZip3,
  ZipWith, sZipWith, ZipWith3, sZipWith3,
  Unzip, sUnzip, Unzip3, sUnzip3,
  -- ** Functions on @Symbol@s
  {-
  The `lines` and `words` functions cannot yet be promoted.
  See the comments in `Data.List.Singletons.Internal`.
  -}
  -- lines, words,
  Unlines, sUnlines, Unwords, sUnwords,

  -- * Converting to and from @Symbol@
  -- ** Converting to @Symbol@
  SymbolS,
  show_,
  PShow(ShowsPrec, ShowList, Show_), SShow(sShowsPrec, sShowList, sShow_),
  Shows, sShows,
  ShowChar, sShowChar, ShowString, sShowString, ShowParen, sShowParen,
  {-
  We do not currently promote or single the Read class.

  -- ** Converting from @Symbol@
  ReadS,
  Read(readsPrec, readList),
  reads, readParen, read, lex,
  -}

  {-
  We do not promote or single anything involving IO.

  -- * Basic Input and output
  IO,
  -- ** Simple I\/O operations
  -- All I/O functions defined here are character oriented.  The
  -- treatment of the newline character will vary on different systems.
  -- For example, two characters of input, return and linefeed, may
  -- read as a single newline character.  These functions cannot be
  -- used portably for binary I/O.
  -- *** Output functions
  putChar,
  putStr, putStrLn, print,
  -- *** Input functions
  getChar,
  getLine, getContents, interact,
  -- *** Files
  FilePath,
  readFile, writeFile, appendFile, readIO, readLn,
  -- ** Exception handling in the I\/O monad
  IOError, ioError, userError,
  -}

  -- * Defunctionalization symbols

  -- ** Basic data types
  FalseSym0, TrueSym0,
  IfSym0, IfSym1, IfSym2, IfSym3,
  type (&&@#@$), type (&&@#@$$), type (&&@#@$$$),
  type (||@#@$), type (||@#@$$), type (||@#@$$$),
  NotSym0, NotSym1,
  OtherwiseSym0,

  NothingSym0, JustSym0, JustSym1,
  Maybe_Sym0, Maybe_Sym1, Maybe_Sym2, Maybe_Sym3,

  LeftSym0, LeftSym1, RightSym0, RightSym1,
  Either_Sym0, Either_Sym1, Either_Sym2, Either_Sym3,

  LTSym0, EQSym0, GTSym0,
  (:@#@$), (:@#@$$), (:@#@$$$), NilSym0,

  -- *** Tuples
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

  -- ** Basic type classes
  type (==@#@$), type (==@#@$$), type (==@#@$$$),
  type (/=@#@$), type (/=@#@$$), type (/=@#@$$$),

  CompareSym0, CompareSym1, CompareSym2,
  type (<@#@$),  type (<@#@$$),  type (<@#@$$$),
  type (<=@#@$), type (<=@#@$$), type (<=@#@$$$),
  type (>@#@$),  type (>@#@$$),  type (>@#@$$$),
  type (>=@#@$), type (>=@#@$$), type (>=@#@$$$),
  MaxSym0, MaxSym1, MaxSym2,
  MinSym0, MinSym1, MinSym2,

  ToEnumSym0, ToEnumSym1,
  FromEnumSym0, FromEnumSym1,
  EnumFromToSym0, EnumFromToSym1, EnumFromToSym2,
  EnumFromThenToSym0, EnumFromThenToSym1, EnumFromThenToSym2, EnumFromThenToSym3,

  MinBoundSym0, MaxBoundSym0,

  -- ** Numbers

  -- *** Numeric type classes
  type (+@#@$), type (+@#@$$), type (+@#@$$$),
  type (-@#@$), type (-@#@$$), type (-@#@$$$),
  type (*@#@$), type (*@#@$$), type (*@#@$$$),
  NegateSym0, NegateSym1,
  AbsSym0, AbsSym1,
  SignumSym0, SignumSym1,
  FromIntegerSym0, FromIntegerSym1,

  -- *** Numeric functions
  SubtractSym0, SubtractSym1, SubtractSym2,

  -- ** Semigroups and Monoids
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MconcatSym0, MconcatSym1,

  -- ** Monads and functors
  FmapSym0, FmapSym1, FmapSym2,
  type (<$@#@$),  type (<$@#@$$),  type (<$@#@$$$),
  type (<$>@#@$), type (<$>@#@$$), type (<$>@#@$$$),
  PureSym0, PureSym1,
  type (<*>@#@$), type (<*>@#@$$), type (<*>@#@$$$),
  type (*>@#@$),  type (*>@#@$$),  type (*>@#@$$$),
  type (<*@#@$),  type (<*@#@$$),  type (<*@#@$$$),
  LiftA2Sym0, LiftA2Sym1, LiftA2Sym2, LiftA2Sym3,
  type (>>=@#@$), type (>>=@#@$$), type (>>=@#@$$$),
  type (>>@#@$),  type (>>@#@$$),  type (>>@#@$$$),
  ReturnSym0, ReturnSym1, FailSym0, FailSym1,
  MapM_Sym0, MapM_Sym1, MapM_Sym2,
  Sequence_Sym0, Sequence_Sym1,
  type (=<<@#@$), type (=<<@#@$$), type (=<<@#@$$$),

  -- ** Folds and traversals
  ElemSym0, ElemSym1, ElemSym2,
  FoldMapSym0, FoldMapSym1, FoldMapSym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  Foldr1Sym0, Foldr1Sym1, Foldr1Sym2,
  Foldl1Sym0, Foldl1Sym1, Foldl1Sym2,
  MaximumSym0, MaximumSym1,
  MinimumSym0, MinimumSym1,
  ProductSym0, ProductSym1,
  SumSym0, SumSym1,

  TraverseSym0, TraverseSym1, TraverseSym2,
  SequenceASym0, SequenceASym1,
  MapMSym0, MapMSym1, MapMSym2,
  SequenceSym0, SequenceSym1,

  -- ** Miscellaneous functions
  IdSym0, IdSym1, ConstSym0, ConstSym1, ConstSym2,
  type (.@#@$),  type (.@#@$$),  type (.@#@$$$), type (.@#@$$$$),
  FlipSym0, FlipSym1, FlipSym2, FlipSym3,
  type ($@#@$),  type ($@#@$$),  type ($@#@$$$),
  UntilSym0, UntilSym1, UntilSym2, UntilSym3,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2,
  ErrorSym0, ErrorSym1,
  ErrorWithoutStackTraceSym0, ErrorWithoutStackTraceSym1,
  UndefinedSym0,
  SeqSym0, SeqSym1, SeqSym2,
  type ($!@#@$), type ($!@#@$$), type ($!@#@$$$),

  -- * List operations
  MapSym0, MapSym1, MapSym2,
  type (++@#@$), type (++@#@$$), type (++@#@$$$),
  FilterSym0, FilterSym1, FilterSym2,
  HeadSym0, HeadSym1, LastSym0, LastSym1,
  TailSym0, TailSym1, InitSym0, InitSym1,
  type (!!@#@$), type (!!@#@$$), type (!!@#@$$$),
  NullSym0, NullSym1,
  LengthSym0, LengthSym1,
  ReverseSym0, ReverseSym1,
  -- *** Special folds
  AndSym0, AndSym1, OrSym0, OrSym1,
  AnySym0, AnySym1, AnySym2,
  AllSym0, AllSym1, AllSym2,
  ConcatSym0, ConcatSym1,
  ConcatMapSym0, ConcatMapSym1, ConcatMapSym2,
  -- ** Building lists
  -- *** Scans
  ScanlSym0, ScanlSym1, ScanlSym2, ScanlSym3,
  Scanl1Sym0, Scanl1Sym1, Scanl1Sym2,
  ScanrSym0, ScanrSym1, ScanrSym2, ScanrSym3,
  Scanr1Sym0, Scanr1Sym1, Scanr1Sym2,
  -- *** Infinite lists
  ReplicateSym0, ReplicateSym1, ReplicateSym2,
  -- ** Sublists
  TakeSym0, TakeSym1, TakeSym2,
  DropSym0, DropSym1, DropSym2,
  TakeWhileSym0, TakeWhileSym1, TakeWhileSym2,
  DropWhileSym0, DropWhileSym1, DropWhileSym2,
  DropWhileEndSym0, DropWhileEndSym1, DropWhileEndSym2,
  SpanSym0, SpanSym1, SpanSym2,
  BreakSym0, BreakSym1, BreakSym2,
  SplitAtSym0, SplitAtSym1, SplitAtSym2,
  -- ** Searching lists
  NotElemSym0, NotElemSym1, NotElemSym2,
  LookupSym0, LookupSym1, LookupSym2,
  -- ** Zipping and unzipping lists
  ZipSym0, ZipSym1, ZipSym2,
  Zip3Sym0, Zip3Sym1, Zip3Sym2, Zip3Sym3,
  ZipWithSym0, ZipWithSym1, ZipWithSym2, ZipWithSym3,
  ZipWith3Sym0, ZipWith3Sym1, ZipWith3Sym2, ZipWith3Sym3,
  UnzipSym0, UnzipSym1, Unzip3Sym0, Unzip3Sym1,
  -- ** Functions on @Symbol@s
  UnlinesSym0, UnlinesSym1, UnwordsSym0, UnwordsSym1,

  -- * Converting to and from @Symbol@
  -- ** Converting to @Symbol@
  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  ShowListSym0, ShowListSym1, ShowListSym2,
  Show_Sym0, Show_Sym1,
  ShowsSym0, ShowsSym1, ShowsSym2,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,
  ShowParenSym0, ShowParenSym1, ShowParenSym2
  ) where

import Control.Applicative.Singletons
  hiding (Const, ConstSym0, ConstSym1)
import Control.Monad.Singletons
import Data.Bool.Singletons
import Data.Either.Singletons
import Data.Eq.Singletons
import Data.Foldable.Singletons
import Data.List.Singletons
import Data.Maybe.Singletons
import Data.Monoid.Singletons
       ( PMonoid(..), SMonoid(..), MemptySym0, MappendSym0
       , MappendSym1, MappendSym2, MconcatSym0, MconcatSym1 )
import Data.Ord.Singletons
import Data.Semigroup.Singletons
       ( PSemigroup(..), SSemigroup(..)
       , type (<>@#@$), type (<>@#@$$), type (<>@#@$$$) )
import Data.Singletons
import Data.Singletons.Base.Enum
  hiding (Succ, Pred, SuccSym0, SuccSym1, PredSym0, PredSym1, sSucc, sPred)
import Data.Traversable.Singletons
import Data.Tuple.Singletons
import GHC.Base.Singletons
  hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFoldr)
import GHC.Num.Singletons
import GHC.TypeLits.Singletons
import Text.Show.Singletons
