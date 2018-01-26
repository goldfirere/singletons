-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Monoid
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a promoted version of 'Monoid'.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Monoid (
  PMonoid(..),

  -- ** Defunctionalization symbols
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  MconcatSym0, MconcatSym1
  ) where

import Data.Singletons.Prelude.Monoid
