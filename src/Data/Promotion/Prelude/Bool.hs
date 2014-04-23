-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Bool
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Re-export promoted definitions related to Bool datatype.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Bool (
  If, Not, (:&&), (:||), Bool_, Otherwise,

  -- * Defunctionalization symbols
  TrueSym0, FalseSym0,

  NotSym0, NotSym1,
  (:&&$), (:&&$$), (:&&$$$),
  (:||$), (:||$$), (:||$$$),
  Bool_Sym0, Bool_Sym1, Bool_Sym2, Bool_Sym3,
  OtherwiseSym0
  ) where

import Data.Singletons.Prelude.Bool
