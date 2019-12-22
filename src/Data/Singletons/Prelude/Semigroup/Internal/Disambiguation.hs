{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Semigroup.Internal.Disambiguation
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- TODO RGS
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Semigroup.Internal.Disambiguation where

import Data.Semigroup
import Data.Singletons.Prelude.Semigroup.Internal
import Data.Singletons.Single

-- We need these in Data.Singletons.Prelude.Foldable.
$(singletons [d|
  all_ :: Bool -> All
  all_ = All

  any_ :: Bool -> Any
  any_ = Any

  sum_ :: a -> Sum a
  sum_ = Sum

  product_ :: a -> Product a
  product_ = Product
  |])
