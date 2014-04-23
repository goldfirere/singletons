-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Either
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jan.stolarek@p.lodz.pl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Re-export promoted definitions related to Either datatype.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Either (
  either_,

  Either_, Lefts, Rights, PartitionEithers, IsLeft, IsRight,

  -- * Defunctionalization symbols
  LeftSym0, LeftSym1, RightSym0, RightSym1,

  Either_Sym0, Either_Sym1, Either_Sym2, Either_Sym3,
  LeftsSym0, LeftsSym1, RightsSym0, RightsSym1,
  IsLeftSym0, IsLeftSym1, IsRightSym0, IsRightSym1
  ) where

import Data.Singletons.Prelude.Either
