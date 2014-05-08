{-# LANGUAGE CPP, TemplateHaskell, KindSignatures, PolyKinds, TypeOperators,
             DataKinds, ScopedTypeVariables, TypeFamilies, GADTs,
             UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Base
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements singletonized functions from GHC.Base module.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Base (

  Until,

  -- * Defunctionalization symbols
  UntilSym0, UntilSym1, UntilSym2, UntilSym3,

  -- * Re-exported from Data.Singletons.Prelude.Base
  Foldr, Map, (:++), Otherwise, Id, Const, (:.), type ($), type ($!),
  Flip, AsTypeOf, Seq,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  MapSym0, MapSym1, MapSym2,
  (:++$), (:++$$),
  OtherwiseSym0,
  IdSym0, IdSym1,
  ConstSym0, ConstSym1, ConstSym2,
  (:.$), (:.$$), (:.$$$),
  type ($$), type ($$$), type ($$$$),
  type ($!$), type ($!$$), type ($!$$$),
  FlipSym0, FlipSym1, FlipSym2,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2,
  SeqSym0, SeqSym1, SeqSym2
  ) where

import Data.Singletons.TH
import Data.Singletons.Prelude.Base

$(promoteOnly [d|
  -- Does not singletoznize. See #30
  until                   :: (a -> Bool) -> (a -> a) -> a -> a
  until p f = go
    where
      go x | p x          = x
           | otherwise    = go (f x)
 |])
