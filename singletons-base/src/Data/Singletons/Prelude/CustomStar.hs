-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.CustomStar
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file implements 'singletonStar', which generates a datatype @Rep@ and associated
-- singleton from a list of types. The promoted version of @Rep@ is kind @*@ and the
-- Haskell types themselves. This is still very experimental, so expect unusual
-- results!
--
-- See also "Data.Singletons.CustomStar" from @singletons-th@, a
-- more minimal version of this module that does not re-export anything from
-- @Data.Singletons.Prelude@.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.CustomStar (
  singletonStar,

  module Data.Singletons.Prelude.Eq,
  module Data.Singletons.Prelude.Bool,
  module Data.Singletons.Prelude.TH
  ) where

import Data.Singletons.CustomStar
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.TH
