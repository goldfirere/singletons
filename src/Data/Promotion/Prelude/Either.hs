-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Either
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jan.stolarek@p.lodz.pl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted functions and datatypes relating to 'Either',
-- including a promoted version of all the definitions in @Data.Either@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Either@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Either (
  -- * Promoted functions from @Data.Either@
  either_, Either_,
  -- | The preceding two definitions are derived from the function 'either' in
  -- @Data.Either@. The extra underscore is to avoid name clashes with the type
  -- 'Either'.

  Lefts, Rights, PartitionEithers, IsLeft, IsRight,

  -- * Defunctionalization symbols
  LeftSym0, LeftSym1, RightSym0, RightSym1,

  Either_Sym0, Either_Sym1, Either_Sym2, Either_Sym3,
  LeftsSym0, LeftsSym1, RightsSym0, RightsSym1,
  IsLeftSym0, IsLeftSym1, IsRightSym0, IsRightSym1
  ) where

import Data.Singletons.Prelude.Either
