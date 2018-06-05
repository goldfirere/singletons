-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.IsString
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports a promoted version of the 'IsString'
-- type class from "Data.String".
----------------------------------------------------------------------------

module Data.Promotion.Prelude.IsString (
  PIsString(..),

  -- ** Defunctionalization symbols
  FromStringSym0, FromStringSym1
  ) where

import Data.Singletons.Prelude.IsString
import Data.Singletons.TypeLits ()   -- for the IsString instance!
