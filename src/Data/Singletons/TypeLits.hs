{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeInType, ConstraintKinds,
             GADTs, TypeFamilies #-}

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
--
----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Singletons.TypeLits (
  Nat, Symbol,
  Sing(SNat, SSym),
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, ErrorSym0, ErrorSym1, sError,
  KnownNat, KnownNatSym0, KnownNatSym1, natVal,
  KnownSymbol, KnownSymbolSym0, KnownSymbolSym1, symbolVal,

  (:^), (:^$), (:^$$), (:^$$$)
  ) where

import Data.Singletons.TypeLits.Internal
import Data.Singletons.Prelude.Num ()   -- for typelits instances

import Data.Singletons.Promote

-- This bogus Num instance is helpful for people who want to define
-- functions over Nats that will only be used at the type level or
-- as singletons. A correct SNum instance for Nat singletons exists.
instance Num Nat where
  (+)         = no_term_level_nats
  (-)         = no_term_level_nats
  (*)         = no_term_level_nats
  negate      = no_term_level_nats
  abs         = no_term_level_nats
  signum      = no_term_level_nats
  fromInteger = no_term_level_nats

instance Eq Nat where
  (==)        = no_term_level_nats

instance Ord Nat where
  compare     = no_term_level_nats

no_term_level_nats :: a
no_term_level_nats = error "The kind `Nat` may not be used at the term level."

-- These are often useful in TypeLits-heavy code
$(genDefunSymbols [''KnownNat, ''KnownSymbol])
