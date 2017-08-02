{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeInType, ConstraintKinds,
             GADTs, TypeFamilies, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeLits
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports singletons useful for the Nat and Symbol kinds.
--
----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Singletons.TypeLits (
  Nat, Symbol,
  Sing(SNat, SSym),
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, sError,
  Undefined, sUndefined,
  KnownNat, natVal,
  KnownSymbol, symbolVal,

  type (^), (%^),
  type (<>), (%<>),

  Div, Mod, DivMod,

  -- * Defunctionalization symbols
  ErrorSym0, ErrorSym1, UndefinedSym0,
  KnownNatSym0, KnownNatSym1,
  KnownSymbolSym0, KnownSymbolSym1,
  type (^@#@$), type (^@#@$$), type (^@#@$$$),
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  DivSym0, DivSym1, DivSym2,
  ModSym0, ModSym1, ModSym2,
  DivModSym0, DivModSym1, DivModSym2
  ) where

import Data.Singletons.TypeLits.Internal
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Tuple

import Data.Singletons.Promote

-- | This bogus 'Num' instance is helpful for people who want to define
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

-- | This bogus instance is helpful for people who want to define
-- functions over Symbols that will only be used at the type level or
-- as singletons.
instance Eq Symbol where
  (==)        = no_term_level_syms

instance Ord Symbol where
  compare     = no_term_level_syms


no_term_level_nats :: a
no_term_level_nats = error "The kind `Nat` may not be used at the term level."

no_term_level_syms :: a
no_term_level_syms = error "The kind `Symbol` may not be used at the term level."

-- These are often useful in TypeLits-heavy code
$(genDefunSymbols [''KnownNat, ''KnownSymbol])

------------------------------------------------------------
-- Div, Mod, DivMod type families.
------------------------------------------------------------
$(promoteOnly [d|
  -- https://ghc.haskell.org/trac/ghc/ticket/13652 asks for these in GHC.TypeLits.
  -- That would be nice, since this implementation is horribly slow.
  divMod :: Nat -> Nat -> (Nat, Nat)
  divMod _ 0 = error "Division by zero."
  divMod x y =
    let (d, m) = (divMod' x (y-1) 0 (y-1))
    in (d, (y-1) - m)

  divMod' :: Nat -> Nat -> Nat -> Nat -> (Nat, Nat)
  divMod' 0 _ q u = (q, u)
  divMod' n y q 0 = divMod' (n-1) y (q+1) y
  divMod' n y q u = divMod' (n-1) y q     (u-1)

  div :: Nat -> Nat -> Nat
  div _ 0 = error "Division by zero."
  div x y = fst (divMod x y)

  mod :: Nat -> Nat -> Nat
  mod _ 0 = error "Division by zero."
  mod x y = snd (divMod x y)

  |])
  