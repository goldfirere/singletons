{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.TypeLits.Singletons
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports singletons useful for the Natural, Symbol, and Char
-- kinds.
--
----------------------------------------------------------------------------

module GHC.TypeLits.Singletons (
  Natural, Symbol, Char,
  Sing,
  SNat, pattern SNat,
  SSymbol, pattern SSymbol, pattern SSym,
  SChar, pattern SChar,
  withKnownNat, withKnownSymbol, withKnownChar,
  Error, sError,
  ErrorWithoutStackTrace, sErrorWithoutStackTrace,
  Undefined, sUndefined,
  KnownNat, natVal,
  KnownSymbol, symbolVal,
  KnownChar, charVal,

  type (^), (%^),
  type (<=?), (%<=?),

  TN.Log2, sLog2,
  Div, sDiv, Mod, sMod, DivMod, sDivMod,
  Quot, sQuot, Rem, sRem, QuotRem, sQuotRem,

  consSymbol, ConsSymbol, sConsSymbol,
  unconsSymbol, UnconsSymbol, sUnconsSymbol,
  charToNat, CharToNat, sCharToNat,
  natToChar, NatToChar, sNatToChar,

  -- * Defunctionalization symbols
  ErrorSym0, ErrorSym1,
  ErrorWithoutStackTraceSym0, ErrorWithoutStackTraceSym1,
  UndefinedSym0,
  KnownNatSym0, KnownNatSym1,
  KnownSymbolSym0, KnownSymbolSym1,
  KnownCharSym0, KnownCharSym1,
  type (^@#@$), type (^@#@$$), type (^@#@$$$),
  type (<=?@#@$), type (<=?@#@$$), type (<=?@#@$$$),
  Log2Sym0, Log2Sym1,
  DivSym0, DivSym1, DivSym2,
  ModSym0, ModSym1, ModSym2,
  DivModSym0, DivModSym1, DivModSym2,
  QuotSym0, QuotSym1, QuotSym2,
  RemSym0, RemSym1, RemSym2,
  QuotRemSym0, QuotRemSym1, QuotRemSym2,
  ConsSymbolSym0, ConsSymbolSym1, ConsSymbolSym2,
  UnconsSymbolSym0, UnconsSymbolSym1,
  CharToNatSym0, CharToNatSym1,
  NatToCharSym0, NatToCharSym1
  ) where

import Data.Char (chr, ord)
import qualified Data.List as L (uncons)
import Data.Singletons
import Data.Singletons.TH
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Tuple.Singletons
import GHC.TypeLits ( CharToNat, ConsSymbol, NatToChar, UnconsSymbol
                    , withSomeSChar, withSomeSSymbol )
import GHC.TypeLits.Singletons.Internal
import qualified GHC.TypeNats as TN
import GHC.TypeNats (Div, Mod)
import Unsafe.Coerce

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

instance Show Symbol where
  showsPrec = no_term_level_syms

no_term_level_syms :: a
no_term_level_syms = error "The kind `Symbol` may not be used at the term level."

-- These are often useful in TypeLits-heavy code
$(genDefunSymbols [''KnownNat, ''KnownSymbol, ''KnownChar])

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
    let x = fromSing sx
    in case x of
         0 -> error "log2 of 0"
         _ -> TN.withSomeSNat (genLog2 x) unsafeCoerce
$(genDefunSymbols [''TN.Log2])
instance SingI Log2Sym0 where
  sing = singFun1 sLog2

sDiv :: Sing x -> Sing y -> Sing (Div x y)
sDiv sx sy =
    let x = fromSing sx
        y = fromSing sy
    in TN.withSomeSNat (x `div` y) unsafeCoerce
infixl 7 `sDiv`
$(genDefunSymbols [''Div])
instance SingI DivSym0 where
  sing = singFun2 sDiv
instance SingI x => SingI (DivSym1 x) where
  sing = singFun1 $ sDiv (sing @x)
instance SingI1 DivSym1 where
  liftSing s = singFun1 $ sDiv s

sMod :: Sing x -> Sing y -> Sing (Mod x y)
sMod sx sy =
    let x = fromSing sx
        y = fromSing sy
    in TN.withSomeSNat (x `mod` y) unsafeCoerce
infixl 7 `sMod`
$(genDefunSymbols [''Mod])
instance SingI ModSym0 where
  sing = singFun2 sMod
instance SingI x => SingI (ModSym1 x) where
  sing = singFun1 $ sMod $ sing @x
instance SingI1 ModSym1 where
  liftSing s = singFun1 $ sMod s

$(promoteOnly [d|
  divMod :: Natural -> Natural -> (Natural, Natural)
  divMod x y = (div x y, mod x y)

  quotRem :: Natural -> Natural -> (Natural, Natural)
  quotRem = divMod

  quot :: Natural -> Natural -> Natural
  quot = div
  infixl 7 `quot`

  rem :: Natural -> Natural -> Natural
  rem = mod
  infixl 7 `rem`
  |])

sDivMod :: Sing x -> Sing y -> Sing (DivMod x y)
sDivMod sx sy =
    let x     = fromSing sx
        y     = fromSing sy
        (q,r) = x `divMod` y
    in TN.withSomeSNat q $ \sq ->
       TN.withSomeSNat r $ \sr ->
       unsafeCoerce (STuple2 sq sr)

sQuotRem :: Sing x -> Sing y -> Sing (QuotRem x y)
sQuotRem = sDivMod

sQuot :: Sing x -> Sing y -> Sing (Quot x y)
sQuot = sDiv
infixl 7 `sQuot`

sRem :: Sing x -> Sing y -> Sing (Rem x y)
sRem = sMod
infixl 7 `sRem`

consSymbol :: Char -> String -> String
consSymbol = (:)

sConsSymbol :: Sing x -> Sing y -> Sing (ConsSymbol x y)
sConsSymbol sx sy =
    let x = fromSing sx
        y = T.unpack (fromSing sy)
    in withSomeSSymbol (consSymbol x y) unsafeCoerce
$(genDefunSymbols [''ConsSymbol])
instance SingI ConsSymbolSym0 where
  sing = singFun2 sConsSymbol
instance SingI x => SingI (ConsSymbolSym1 x) where
  sing = singFun1 $ sConsSymbol $ sing @x
instance SingI1 ConsSymbolSym1 where
  liftSing s = singFun1 $ sConsSymbol s

unconsSymbol :: String -> Maybe (Char, String)
unconsSymbol = L.uncons

sUnconsSymbol :: Sing x -> Sing (UnconsSymbol x)
sUnconsSymbol sx =
    let x   = T.unpack (fromSing sx)
        res = toSing (unconsSymbol x)
    in case res of
         SomeSing s -> unsafeCoerce s
$(genDefunSymbols [''UnconsSymbol])
instance SingI UnconsSymbolSym0 where
  sing = singFun1 sUnconsSymbol

charToNat :: Char -> Natural
charToNat = fromIntegral . ord

sCharToNat :: Sing x -> Sing (CharToNat x)
sCharToNat sx =
    let x = fromSing sx
    in TN.withSomeSNat (charToNat x) unsafeCoerce
$(genDefunSymbols [''CharToNat])
instance SingI CharToNatSym0 where
  sing = singFun1 sCharToNat

natToChar :: Natural -> Char
natToChar = chr . fromIntegral

sNatToChar :: Sing x -> Sing (NatToChar x)
sNatToChar sx =
    let x = fromSing sx
    in withSomeSChar (natToChar x) unsafeCoerce
$(genDefunSymbols [''NatToChar])
instance SingI NatToCharSym0 where
  sing = singFun1 sNatToChar
