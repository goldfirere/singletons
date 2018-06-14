-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Const
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports defunctionalization symbosl for the 'Const' data type.
--
-----------------------------------------------------------------------------

module Data.Promotion.Prelude.Const (
  -- * Defunctionalization symbols
  ConstSym0, ConstSym1,
  GetConstSym0, GetConstSym1
  ) where

import Data.Singletons.Prelude.Const
