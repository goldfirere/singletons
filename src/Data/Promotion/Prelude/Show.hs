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
  PShow(..), SymbolS, SChar, show_, (:<>),
  Shows, ShowListWith, ShowChar, ShowString, ShowParen,
  ShowSpace, ShowCommaSpace, AppPrec, AppPrec1,

  -- * Defunctionalization symbols
  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  Show_Sym0, Show_Sym1,
  ShowListSym0, ShowListSym1, ShowListSym2,
  (:<>$), (:<>$$), (:<>$$$),
  ShowsSym0, ShowsSym1, ShowsSym2,
  ShowListWithSym0, ShowListWithSym1, ShowListWithSym2, ShowListWithSym3,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,
  ShowSpaceSym0, ShowSpaceSym1,
  ShowCommaSpaceSym0, ShowCommaSpaceSym1,
  AppPrecSym0, AppPrec1Sym0
  ) where

import Data.Singletons.Prelude.Show
