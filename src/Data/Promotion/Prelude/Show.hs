-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Show
-- Copyright   :  (C) 2014 Jan Stolarek, Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a promoted version of 'Show'
--
-----------------------------------------------------------------------------

module Data.Promotion.Prelude.Show (
  PShow(..), SymbolS, SChar,

  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  Show'Sym0, Show'Sym1,
  ShowListSym0, ShowListSym1, ShowListSym2,

  (:<>),
  (:<>$), (:<>$$), (:<>$$$),

  Shows,
  ShowsSym0, ShowsSym1, ShowsSym2,

  ShowListWith,
  ShowListWithSym0, ShowListWithSym1, ShowListWithSym2, ShowListWithSym3,

  ShowChar,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,

  ShowString,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,

  ShowParen,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,

  ShowSpace,
  ShowSpaceSym0, ShowSpaceSym1,

  ShowCommaSpace,
  ShowCommaSpaceSym0, ShowCommaSpaceSym1,

  AppPrec, AppPrecSym0,
  AppPrec1, AppPrec1Sym0
  ) where

import Data.Singletons.Prelude.Show
