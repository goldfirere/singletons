{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Decide
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the class 'SDecide', allowing for decidable equality over singletons.
--
----------------------------------------------------------------------------

module Data.Singletons.Decide (
  -- * The SDecide class
  SDecide(..),

  -- * Supporting definitions
  (:~:)(..), Void, Refuted, Decision(..)
  ) where

import Data.Singletons.Types
import Data.Singletons.Core
import Data.Singletons.Void

#if __GLASGOW_HASKELL__ >= 707
import Data.Type.Equality
#endif
