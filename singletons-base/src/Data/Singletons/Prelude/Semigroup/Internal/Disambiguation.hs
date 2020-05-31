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
-- Module      :  Data.Singletons.Prelude.Semigroup.Internal.Disambiguation
-- Copyright   :  (C) 2019 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Provides aliases for 'All', 'Any', 'Sum', and 'Product' that do not clash
-- with the promoted functions of the same names in
-- Data.Singletons.Prelude.Foldable.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Semigroup.Internal.Disambiguation where

import Data.Semigroup
import Data.Singletons.Prelude.Semigroup.Internal
import Data.Singletons.TH

-- We need these in Data.Singletons.Prelude.Foldable, as we need to promote
-- code that simultaneously uses the All/Any/Sum/Product constructors and the
-- all/any/sum/product functions, which have clashing defunctionalization
-- symbol names. Our workaround is to simply define synonyms for
-- all/any/sum/product and use those instead.
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
