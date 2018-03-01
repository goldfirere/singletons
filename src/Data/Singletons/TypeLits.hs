{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeInType, ConstraintKinds,
             GADTs, TypeApplications, TypeFamilies, UndecidableInstances #-}

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
  ErrorWithoutStackTrace, sErrorWithoutStackTrace,
  Undefined, sUndefined,
  KnownNat, natVal,
  KnownSymbol, symbolVal,

  type (^), (%^),
  type (<=?), (%<=?),

  TN.Log2, sLog2,
  Div, sDiv, Mod, sMod, DivMod, sDivMod,
  Quot, sQuot, Rem, sRem, QuotRem, sQuotRem,

  -- * Defunctionalization symbols
  ErrorSym0, ErrorSym1,
  ErrorWithoutStackTraceSym0, ErrorWithoutStackTraceSym1,
  UndefinedSym0,
  KnownNatSym0, KnownNatSym1,
  KnownSymbolSym0, KnownSymbolSym1,
  type (^@#@$), type (^@#@$$), type (^@#@$$$),
  type (<=?@#@$), type (<=?@#@$$), type (<=?@#@$$$),
  Log2Sym0, Log2Sym1,
  DivSym0, DivSym1, DivSym2,
  ModSym0, ModSym1, ModSym2,
  DivModSym0, DivModSym1, DivModSym2,
  QuotSym0, QuotSym1, QuotSym2,
  RemSym0, RemSym1, RemSym2,
  QuotRemSym0, QuotRemSym1, QuotRemSym2
  ) where

import Data.Singletons.Internal
import Data.Singletons.Prelude.Tuple
import Data.Singletons.Promote
import Data.Singletons.ShowSing ()      -- for ShowSing/Show instances
import Data.Singletons.TypeLits.Internal

import Data.String (IsString(..))
import qualified GHC.TypeNats as TN
import GHC.TypeNats (Div, Mod, SomeNat(..))
import Numeric.Natural (Natural)

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

instance Semigroup Symbol where
  (<>) = no_term_level_syms

instance Monoid Symbol where
  mempty = no_term_level_syms

no_term_level_nats :: a
no_term_level_nats = error "The kind `Nat` may not be used at the term level."

no_term_level_syms :: a
no_term_level_syms = error "The kind `Symbol` may not be used at the term level."

-- These are often useful in TypeLits-heavy code
$(genDefunSymbols [''KnownNat, ''KnownSymbol])

------------------------------------------------------------
-- Log2, Div, Mod, DivMod, and friends
------------------------------------------------------------

{- | Adapted from GHC's source code.

Compute the logarithm of a number in the given base, rounded down to the
closest integer. -}
genLog2 :: Natural -> Natural
genLog2 x = exactLoop 0 x
  where
  exactLoop s i
    | i == 1     = s
    | i < 2      = s
    | otherwise  =
        let s1 = s + 1
        in s1 `seq` case divMod i 2 of
                      (j,r)
                        | r == 0    -> exactLoop s1 j
                        | otherwise -> underLoop s1 j

  underLoop s i
    | i < 2  = s
    | otherwise = let s1 = s + 1 in s1 `seq` underLoop s1 (div i 2)


sLog2 :: Sing x -> Sing (TN.Log2 x)
sLog2 sx =
    let x   = fromSing sx
    in case x of
         0 -> error "log2 of 0"
         _ -> case TN.someNatVal (genLog2 x) of
                SomeNat (_ :: Proxy res) -> unsafeCoerce (SNat :: Sing res)
$(genDefunSymbols [''TN.Log2])
instance SingI Log2Sym0 where
  sing = singFun1 sLog2

sDiv :: Sing x -> Sing y -> Sing (Div x y)
sDiv sx sy =
    let x   = fromSing sx
        y   = fromSing sy
        res = TN.someNatVal (x `div` y)
    in case res of
         SomeNat (_ :: Proxy res) -> unsafeCoerce (SNat :: Sing res)
infixl 7 `sDiv`
$(genDefunSymbols [''Div])
instance SingI DivSym0 where
  sing = singFun2 sDiv
instance SingI x => SingI (DivSym1 x) where
  sing = singFun1 $ sDiv (sing @_ @x)

sMod :: Sing x -> Sing y -> Sing (Mod x y)
sMod sx sy =
    let x   = fromSing sx
        y   = fromSing sy
        res = TN.someNatVal (x `mod` y)
    in case res of
         SomeNat (_ :: Proxy res) -> unsafeCoerce (SNat :: Sing res)
infixl 7 `sMod`
$(genDefunSymbols [''Mod])
instance SingI ModSym0 where
  sing = singFun2 sMod
instance SingI x => SingI (ModSym1 x) where
  sing = singFun1 $ sMod $ sing @_ @x

$(promoteOnly [d|
  divMod :: Nat -> Nat -> (Nat, Nat)
  divMod x y = (div x y, mod x y)

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

sQuotRem :: Sing x -> Sing y -> Sing (QuotRem x y)
sQuotRem = sDivMod

sQuot :: Sing x -> Sing y -> Sing (Quot x y)
sQuot = sDiv
infixl 7 `sQuot`

sRem :: Sing x -> Sing y -> Sing (Rem x y)
sRem = sMod
infixl 7 `sRem`
