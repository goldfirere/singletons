{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Functor
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of the 'Functor' type class.
--
----------------------------------------------------------------------------

module Data.Promotion.Prelude.Functor (
  PFunctor(..),
  type ($>), type (<$>), type (<&>), Void,

  -- * Defunctionalization symbols
  FmapSym0, FmapSym1, FmapSym2,
  type (<$@#@$),  type (<$@#@$$),  type (<$@#@$$$),
  type ($>@#@$),  type ($>@#@$$),  type ($>@#@$$$),
  type (<$>@#@$), type (<$>@#@$$), type (<$>@#@$$$),
  type (<&>@#@$), type (<&>@#@$$), type (<&>@#@$$$),
  VoidSym0, VoidSym1
  ) where

import Data.Singletons.Prelude.Functor
