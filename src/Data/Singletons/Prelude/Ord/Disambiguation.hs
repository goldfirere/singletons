{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Ord.Disambiguation
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Provides aliases for 'Min' and 'Max' that do not clash with the data
-- types of the same names in Data.Singletons.Prelude.Semigroup.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Ord.Disambiguation where

import Data.Singletons.Prelude.Ord
import Data.Singletons.Single

-- We need these in Data.Singletons.Prelude.Semigroup, as we need to promote
-- code that simultaneously uses the Min/Max constructors and the min/max
-- functions, which have clashing defunctionalization symbol names. Our
-- workaround is to simply define synonyms for min/max and use those instead.
$(singletons [d|
  min_, max_ :: Ord a => a -> a -> a
  min_ = min
  max_ = max
  |])
