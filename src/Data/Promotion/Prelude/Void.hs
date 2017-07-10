-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Void
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jan.stolarek@p.lodz.pl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted functions and datatypes relating to 'Void',
-- including a promoted version of all the definitions in @Data.Void@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Void@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------
module Data.Promotion.Prelude.Void (
  -- * Promoted functions from from @Data.Void@
  Absurd,

  -- * Defunctionalization symbols
  AbsurdSym0, AbsurdSym1
  ) where

import Data.Singletons.Prelude.Void
