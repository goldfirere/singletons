-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Maybe
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Re-export promoted definitions related to Maybe datatype.
--
----------------------------------------------------------------------------

{-# LANGUAGE KindSignatures, PolyKinds, DataKinds, TypeFamilies, GADTs,
             UndecidableInstances #-}
module Data.Promotion.Prelude.Maybe (
  Maybe_, IsJust, IsNothing, FromJust, FromMaybe, MaybeToList,
  ListToMaybe, CatMaybes, MapMaybe,

  -- * Defunctionalization symbols
  NothingSym0, JustSym0, JustSym1,

  Maybe_Sym0, Maybe_Sym1, Maybe_Sym2, Maybe_Sym3,
  IsJustSym0, IsJustSym1, IsNothingSym0, IsNothingSym1,
  FromJustSym0, FromJustSym1, FromMaybeSym0, FromMaybeSym1, FromMaybeSym2,
  MaybeToListSym0, MaybeToListSym1, ListToMaybeSym0, ListToMaybeSym1,
  CatMaybesSym0, CatMaybesSym1, MapMaybeSym0, MapMaybeSym1, MapMaybeSym2
  ) where

import Data.Singletons.Prelude.Maybe
import Data.Promotion.Prelude.Monad
import Data.Singletons.TH

-- Promote Monad instance
$(promoteOnly [d|
    maybeReturn            :: a -> Maybe a
    maybeReturn a          = Just a

    maybeBind              :: Maybe a -> (a -> Maybe a) -> Maybe a
    maybeBind (Just x) k   = k x
    maybeBind Nothing  _   = Nothing

    maybeBind'             :: Maybe a -> Maybe a -> Maybe a
    maybeBind' (Just _) k  = k
    maybeBind' Nothing  _  = Nothing
 |])

type instance Return a = MaybeReturn a
type instance a :>>= b = MaybeBind   a b
type instance a :>>  b = MaybeBind'  a b
