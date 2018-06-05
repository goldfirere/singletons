{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Monad
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of the 'Monad' type class.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Monad (
  PFunctor(Fmap),
  PMonad(..), PMonadPlus(..),

  -- MapM, MapM_, ForM, Sequence, Sequence_,
  type (=<<), type (>=>), type (<=<),
  -- Forever,
  Void,

  Join,
  -- Msum,
  Mfilter, FilterM,
  -- MapAndUnzipM, ZipWithM, ZipWithM_, FoldlM,
  ReplicateM, ReplicateM_,

  Guard, When, Unless,

  LiftM, LiftM2, LiftM3, LiftM4, LiftM5, Ap,

  type (<$!>),

  -- * Defunctionalization symbols
  FmapSym0, FmapSym1, FmapSym2,
  type (>>=@#@$), type (>>=@#@$$), type (>>=@#@$$$),
  type (>>@#@$),  type (>>@#@$$),  type (>>@#@$$$),
  ReturnSym0, ReturnSym1, FailSym0, FailSym1,
  MzeroSym0, MplusSym0, MplusSym1, MplusSym2,

  {-
  MapMSym0,  MapMSym1,  MapMSym2,
  MapM_Sym0, MapM_Sym1, MapM_Sym2,
  ForMSym0,  ForMSym1,  ForMSym2,
  SequenceSym0,  SequenceSym1,
  Sequence_Sym0, Sequence_Sym1,
  -}
  type (=<<@#@$), type (=<<@#@$$), type (=<<@#@$$$),
  type (>=>@#@$), type (>=>@#@$$), type (>=>@#@$$$),
  type (<=<@#@$), type (<=<@#@$$), type (<=<@#@$$$),
  -- ForeverSym0, ForeverSym1,
  VoidSym0, VoidSym1,

  JoinSym0, JoinSym1,
  -- MsumSym0, MsumSym1,
  MfilterSym0, MfilterSym1, MfilterSym2,
  FilterMSym0, FilterMSym1, FilterMSym2,
  {-
  MapAndUnzipMSym0, MapAndUnzipMSym1, MapAndUnzipMSym2,
  ZipWithMSym0,  ZipWithMSym1,  ZipWithMSym2,  ZipWithMSym3,
  ZipWithM_Sym0, ZipWithM_Sym1, ZipWithM_Sym2, ZipWithM_Sym3,
  FoldlMSym0,    FoldlMSym1,    FoldlMSym2,    FoldlMSym3,
  -}
  ReplicateMSym0,  ReplicateMSym1,  ReplicateMSym2,
  ReplicateM_Sym0, ReplicateM_Sym1, ReplicateM_Sym2,

  GuardSym0, GuardSym1,
  WhenSym0, WhenSym1, WhenSym2,
  UnlessSym0, UnlessSym1, UnlessSym2,

  LiftMSym0,  LiftMSym1,  LiftMSym2,
  LiftM2Sym0, LiftM2Sym1, LiftM2Sym2, LiftM2Sym3,
  LiftM3Sym0, LiftM3Sym1, LiftM3Sym2, LiftM3Sym3, LiftM3Sym4,
  LiftM4Sym0, LiftM4Sym1, LiftM4Sym2, LiftM4Sym3, LiftM4Sym4, LiftM4Sym5,
  LiftM5Sym0, LiftM5Sym1, LiftM5Sym2, LiftM5Sym3, LiftM5Sym4, LiftM5Sym5, LiftM5Sym6,
  ApSym0, ApSym1, ApSym2,

  type (<$!>@#@$), type (<$!>@#@$$), type (<$!>@#@$$$),
  ) where

import Data.Singletons.Prelude.Monad
