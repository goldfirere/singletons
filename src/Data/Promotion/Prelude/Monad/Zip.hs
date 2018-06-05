-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Monad.Zip
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of the 'MonadZip' type class.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Monad.Zip (
  PMonadZip(..),

  -- * Defunctionalization symbols
  MzipSym0, MzipSym1, MzipSym2,
  MzipWithSym0, MzipWithSym1, MzipWithSym2, MzipWithSym3,
  MunzipSym0, MunzipSym1,
  ) where

import Data.Singletons.Prelude.Monad.Zip
