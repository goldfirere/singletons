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

{-# LANGUAGE CPP, PolyKinds, DataKinds, TypeFamilies, FlexibleInstances,
             UndecidableInstances, ScopedTypeVariables, RankNTypes,
             GADTs #-}

module Data.Singletons.TypeLits (
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, sError
  ) where

import Data.Singletons
import Data.Singletons.Types
import Data.Singletons.Eq
import Data.Singletons.Decide
import Data.Singletons.Bool
#if __GLASGOW_HASKELL__ >= 707
import GHC.TypeLits
#else
import GHC.TypeLits (Nat, Symbol)
import qualified GHC.TypeLits as TypeLits
#endif
import Unsafe.Coerce

----------------------------------------------------------------------
---- TypeLits singletons ---------------------------------------------
----------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 707
data instance Sing (n :: Nat) = KnownNat n => SNat
instance KnownNat n => SingI n where
  sing = SNat
#else
instance TypeLits.SingRep n Integer => SingI (n :: Nat) where
  sing = SNat (TypeLits.fromSing (TypeLits.sing :: TypeLits.Sing n))
#endif
instance SingKind ('KProxy :: KProxy Nat) where
  type DemoteRep ('KProxy :: KProxy Nat) = Integer
  fromSing (SNat :: Sing n) = natVal (Proxy :: Proxy n)
  toSing n = case someNatVal n of
               Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNat :: Sing n)
               Nothing -> error "Negative singleton nat"

#if __GLASGOW_HASKELL__ >= 707
data instance Sing (n :: Symbol) = KnownSymbol n => SSym
instance KnownSymbol n => SingI n where
  sing = SSym
#else
instance TypeLits.SingRep n String => SingI (n :: Symbol) where
  sing = SSym (TypeLits.fromSing (TypeLits.sing :: TypeLits.Sing n))
#endif
instance SingKind ('KProxy :: KProxy Symbol) where
  type DemoteRep ('KProxy :: KProxy Symbol) = String
  fromSing (SSym :: Sing n) = symbolVal (Proxy :: Proxy n)
  toSing s = case someSymbolVal s of
               SomeSymbol (_ :: Proxy n) -> SomeSing (SSym :: Sing n)

-- We need SDecide instances for the TypeLits singletons
instance SDecide ('KProxy :: KProxy Nat) where
  (SNat :: Sing n) %~ (SNat :: Sing m)
    | natVal (Proxy :: Proxy n) == natVal (Proxy :: Proxy m)
    = Proved $ unsafeCoerce Refl
    | otherwise
    = Disproved (\_ -> error errStr)
    where errStr = "Broken Nat singletons"

instance SDecide ('KProxy :: KProxy Symbol) where
  (SSym :: Sing n) %~ (SSym :: Sing m)
    | symbolVal (Proxy :: Proxy n) == symbolVal (Proxy :: Proxy m)
    = Proved $ unsafeCoerce Refl
    | otherwise
    = Disproved (\_ -> error errStr)
    where errStr = "Broken Symbol singletons"

-- need SEq instances for TypeLits kinds
instance SEq ('KProxy :: KProxy Nat) where
  a %:== b
    | fromSing a == fromSing b    = unsafeCoerce STrue
    | otherwise                   = unsafeCoerce SFalse

instance SEq ('KProxy :: KProxy Symbol) where
  a %:== b
    | fromSing a == fromSing b    = unsafeCoerce STrue
    | otherwise                   = unsafeCoerce SFalse
                  
-- | Kind-restricted synonym for 'Sing' for @Nat@s
type SNat (x :: Nat) = Sing x

-- | Kind-restricted synonym for 'Sing' for @Symbol@s
type SSymbol (x :: Symbol) = Sing x

-- Convenience functions

-- | Given a singleton for @Nat@, call something requiring a
-- @KnownNat@ instance.
withKnownNat :: Sing n -> (KnownNat n => r) -> r
withKnownNat SNat f = f

-- | Given a singleton for @Symbol@, call something requiring
-- a @KnownSymbol@ instance.
withKnownSymbol :: Sing n -> (KnownSymbol n => r) -> r
withKnownSymbol SSym f = f

-- | The promotion of 'error'
type family Error (str :: Symbol) :: k

-- | The singleton for 'error'
sError :: Sing (str :: Symbol) -> a
sError sstr = error (fromSing sstr)
