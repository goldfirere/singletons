{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables,
             TypeFamilies, TypeOperators, GADTs, UndecidableInstances,
             FlexibleContexts, DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Bounded
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of Bounded, 'PBounded'
--
-----------------------------------------------------------------------------

module Data.Promotion.Prelude.Bounded (
  PBounded(..),

  -- ** Defunctionalization symbols
  MaxBoundSym0,
  MinBoundSym0
  ) where

import Data.Singletons.Promote
import Data.Singletons.Util

$(promoteOnly [d|
  class Bounded a  where
    minBound, maxBound :: a
  |])

$(promoteBoundedInstances boundedBasicTypes)
