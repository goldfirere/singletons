{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Semigroup
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Exports a promoted version of 'Semigroup'.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Semigroup (
  PSemigroup(..),

  -- ** Defunctionalization symbols
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  SconcatSym0, SconcatSym1
  ) where

import Data.Singletons.Prelude.Semigroup
