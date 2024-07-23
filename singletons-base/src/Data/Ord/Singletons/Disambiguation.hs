{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ord.Singletons.Disambiguation
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Provides aliases for 'Min' and 'Max' that do not clash with the data
-- types of the same names in "Data.Semigroup.Singletons".
--
----------------------------------------------------------------------------

module Data.Ord.Singletons.Disambiguation where

import Data.Ord.Singletons
import Data.Singletons.TH

-- We need these in Data.Semigroup.Singletons, as we need to promote
-- code that simultaneously uses the Min/Max constructors and the min/max
-- functions, which have clashing defunctionalization symbol names. Our
-- workaround is to simply define synonyms for min/max and use those instead.
$(singletons [d|
  min_, max_ :: Ord a => a -> a -> a
  min_ = min
  max_ = max
  |])
