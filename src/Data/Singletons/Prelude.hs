-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
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
  -- | Though Haddock doesn't show it, the 'Sing' instance above includes
  -- the following instances
  --
  -- > data instance Sing (a :: Bool) where
  -- >   SFalse :: Sing False
  -- >   STrue  :: Sing True
  -- >
  -- > data instance Sing (a :: [k]) where
  -- >   SNil  :: Sing '[]
  -- >   SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)
  -- >
  -- > data instance Sing (a :: Maybe k) where
  -- >   SNothing :: Sing Nothing
  -- >   SJust    :: Sing (a :: k) -> Sing (Just a)
  -- >
  -- > data instance Sing (a :: Either x y) where
  -- >   SLeft  :: Sing (a :: x) -> Sing (Left a)
  -- >   SRight :: Sing (b :: y) -> Sing (Right b)
  -- >
  -- > data instance Sing (a :: Ordering) where
  -- >   SLT :: Sing LT
  -- >   SEQ :: Sing EQ
  -- >   SGT :: Sing GT
  -- >
  -- > data instance Sing (a :: ()) where
  -- >   STuple0 :: Sing '()
  -- >
  -- > data instance Sing (z :: (a, b)) where
  -- >   STuple2 :: Sing a -> Sing b -> Sing '(a, b)
  -- >
  -- > data instance Sing (z :: (a, b, c)) where
  -- >   STuple3 :: Sing a -> Sing b -> Sing c -> Sing '(a, b, c)
  -- >
  -- > data instance Sing (z :: (a, b, c, d)) where
  -- >   STuple4 :: Sing a -> Sing b -> Sing c -> Sing d -> Sing '(a, b, c, d)
  -- >
  -- > data instance Sing (z :: (a, b, c, d, e)) where
  -- >   STuple5 :: Sing a -> Sing b -> Sing c -> Sing d -> Sing e -> Sing '(a, b, c, d, e)
  -- >
  -- > data instance Sing (z :: (a, b, c, d, e, f)) where
  -- >   STuple6 :: Sing a -> Sing b -> Sing c -> Sing d -> Sing e -> Sing f
  -- >           -> Sing '(a, b, c, d, e, f)
  -- >
  -- > data instance Sing (z :: (a, b, c, d, e, f, g)) where
  -- >   STuple7 :: Sing a -> Sing b -> Sing c -> Sing d -> Sing e -> Sing f
  -- >           -> Sing g -> Sing '(a, b, c, d, e, f, g)

  -- * Singleton type synonyms

  -- | These synonyms are all kind-restricted synonyms of 'Sing'.
  -- For example 'SBool' requires an argument of kind 'Bool'.
  SBool, SList, SMaybe, SEither, SOrdering,
  STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,

  -- * Functions working with 'Bool'
  If, sIf, Not, sNot, (:&&), (:||), (%:&&), (%:||), Otherwise, sOtherwise,

  -- * Error reporting
  Error, ErrorSym0, sError,

  -- * Singleton equality
  module Data.Singletons.Prelude.Eq,

  -- * Singleton comparisons
  module Data.Singletons.Prelude.Ord,

  -- ** Miscellaneous functions
  Id, sId, Const, sConst, (:.), (%:.), type ($), (%$), type ($!), (%$!),
  Flip, sFlip, AsTypeOf, sAsTypeOf,
  Seq, sSeq,

  -- * List operations
  Map, sMap, (:++), (%:++), Head, sHead, Last, sLast, Tail, sTail,
  Init, sInit, Null, sNull, Reverse, sReverse,
  -- ** Reducing lists (folds)
  Foldl, sFoldl, Foldl1, sFoldl1, Foldr, sFoldr, Foldr1, sFoldr1,
  -- *** Special folds
  And, sAnd, Or, sOr, Any_, sAny_, All, sAll,
  Concat, sConcat, ConcatMap, sConcatMap,
  -- *** Scans
  Scanl, sScanl, Scanl1, sScanl1, Scanr, sScanr, Scanr1, sScanr1,
  -- ** Searching lists
  Elem, sElem, NotElem, sNotElem,
  -- ** Zipping and unzipping lists
  Zip, sZip, Zip3, sZip3, ZipWith, sZipWith, ZipWith3, sZipWith3,
  Unzip, sUnzip, Unzip3, sUnzip3,

  -- * Other datatypes
  Maybe_, sMaybe_,
  Either_, sEither_,
  Fst, sFst, Snd, sSnd, Curry, sCurry, Uncurry, sUncurry,
  Symbol,

  -- * Other functions
  either_, -- reimplementation of either to be used with singletons library
  maybe_,
  bool_,
  any_,

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

  IdSym0, IdSym1, ConstSym0, ConstSym1, ConstSym2,
  (:.$), (:.$$), (:.$$$),
  type ($$), type ($$$), type ($$$$),
  type ($!$), type ($!$$), type ($!$$$),
  FlipSym0, FlipSym1, FlipSym2,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2, SeqSym0, SeqSym1, SeqSym2,

  (:$), (:$$), (:$$$), NilSym0,
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

import Data.Singletons
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Tuple
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Instances
import Data.Singletons.TypeLits
