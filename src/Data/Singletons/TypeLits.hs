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

  Div, sDiv, Mod, sMod, DivMod, sDivMod,
  Quot, sQuot, Rem, sRem, QuotRem, sQuotRem,

  -- * Defunctionalization symbols
  ErrorSym0, ErrorSym1, UndefinedSym0,
  KnownNatSym0, KnownNatSym1,
  KnownSymbolSym0, KnownSymbolSym1,
  type (^@#@$), type (^@#@$$), type (^@#@$$$),
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  DivSym0, DivSym1, DivSym2,
  ModSym0, ModSym1, ModSym2,
  DivModSym0, DivModSym1, DivModSym2,
  QuotSym0, QuotSym1, QuotSym2,
  RemSym0, RemSym1, RemSym2,
  QuotRemSym0, QuotRemSym1, QuotRemSym2
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Tuple
import Data.Singletons.Promote
import Data.Singletons.ShowSing ()      -- for ShowSing/Show instances
import Data.Singletons.TypeLits.Internal

import Data.String (IsString(..))
import qualified GHC.TypeNats as TN
import GHC.TypeNats (SomeNat(..))

import Unsafe.Coerce

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

instance IsString Symbol where
  fromString  = no_term_level_syms

no_term_level_nats :: a
no_term_level_nats = error "The kind `Nat` may not be used at the term level."

no_term_level_syms :: a
no_term_level_syms = error "The kind `Symbol` may not be used at the term level."

-- These are often useful in TypeLits-heavy code
$(genDefunSymbols [''KnownNat, ''KnownSymbol])

------------------------------------------------------------
-- Div, Mod, DivMod, and friends
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
  infixl 7 `div`

  mod :: Nat -> Nat -> Nat
  mod _ 0 = error "Division by zero."
  mod x y = snd (divMod x y)
  infixl 7 `mod`

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem = divMod

  quot :: Nat -> Nat -> Nat
  quot = div
  infixl 7 `quot`

  rem :: Nat -> Nat -> Nat
  rem = mod
  infixl 7 `rem`
  |])

sDivMod :: Sing x -> Sing y -> Sing (DivMod x y)
sDivMod sx sy =
    let x     = fromSing sx
        y     = fromSing sy
        (q,r) = x `divMod` y
        qRes  = TN.someNatVal q
        rRes  = TN.someNatVal r
    in case (qRes, rRes) of
         (SomeNat (_ :: Proxy q), SomeNat (_ :: Proxy r))
           -> unsafeCoerce (STuple2 (SNat :: Sing q) (SNat :: Sing r))

sDiv :: Sing x -> Sing y -> Sing (Div x y)
sDiv sx sy =
    let x   = fromSing sx
        y   = fromSing sy
        res = TN.someNatVal (x `div` y)
    in case res of
         SomeNat (_ :: Proxy res) -> unsafeCoerce (SNat :: Sing res)
infixl 7 `sDiv`

sMod :: Sing x -> Sing y -> Sing (Mod x y)
sMod sx sy =
    let x   = fromSing sx
        y   = fromSing sy
        res = TN.someNatVal (x `mod` y)
    in case res of
         SomeNat (_ :: Proxy res) -> unsafeCoerce (SNat :: Sing res)
infixl 7 `sMod`

sQuotRem :: Sing x -> Sing y -> Sing (QuotRem x y)
sQuotRem = sDivMod

sQuot :: Sing x -> Sing y -> Sing (Quot x y)
sQuot = sDiv
infixl 7 `sQuot`

sRem :: Sing x -> Sing y -> Sing (Rem x y)
sRem = sMod
infixl 7 `sRem`
