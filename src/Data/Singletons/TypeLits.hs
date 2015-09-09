-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeLits
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports singletons useful for the Nat and Symbol kinds.
-- This exports the internal, unsafe constructors. Use Data.Singletons.TypeLits
-- for a safe interface.
--
----------------------------------------------------------------------------

module Data.Singletons.TypeLits (
  Nat, Symbol,
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, ErrorSym0, ErrorSym1, sError,
  KnownNat, natVal, KnownSymbol, symbolVal,

  (:^), (:^$), (:^$$), (:^$$$)
  ) where

import Data.Singletons.TypeLits.Internal
import Data.Singletons.Prelude.Num ()   -- for typelits instances
