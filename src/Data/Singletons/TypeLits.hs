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
             GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ < 707
{-# OPTIONS_GHC -O0 #-}   -- don't optimize SDecide instances in 7.6!
#endif

module Data.Singletons.TypeLits (
  Nat, Symbol,
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, sError,
  KnownNat, natVal, KnownSymbol, symbolVal
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
import qualified GHC.TypeLits as TL
#endif
import Unsafe.Coerce

----------------------------------------------------------------------
---- TypeLits singletons ---------------------------------------------
----------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 707
data instance Sing (n :: Nat) = KnownNat n => SNat

instance KnownNat n => SingI n where
  sing = SNat

instance SingKind ('KProxy :: KProxy Nat) where
  type DemoteRep ('KProxy :: KProxy Nat) = Integer
  fromSing (SNat :: Sing n) = natVal (Proxy :: Proxy n)
  toSing n = case someNatVal n of
               Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNat :: Sing n)
               Nothing -> error "Negative singleton nat"

data instance Sing (n :: Symbol) = KnownSymbol n => SSym

instance KnownSymbol n => SingI n where
  sing = SSym

instance SingKind ('KProxy :: KProxy Symbol) where
  type DemoteRep ('KProxy :: KProxy Symbol) = String
  fromSing (SSym :: Sing n) = symbolVal (Proxy :: Proxy n)
  toSing s = case someSymbolVal s of
               SomeSymbol (_ :: Proxy n) -> SomeSing (SSym :: Sing n)
                  
#else

data TLSingInstance (a :: k) where
  TLSingInstance :: TL.SingI a => TLSingInstance a

newtype DI a = Don'tInstantiate (TL.SingI a => TLSingInstance a)

tlSingInstance :: forall (a :: k). TL.Sing a -> TLSingInstance a
tlSingInstance s = with_sing_i TLSingInstance
  where
    with_sing_i :: (TL.SingI a => TLSingInstance a) -> TLSingInstance a
    with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

withTLSingI :: TL.Sing n -> (TL.SingI n => r) -> r
withTLSingI sn r =
  case tlSingInstance sn of
    TLSingInstance -> r

data instance Sing (n :: Nat) = TL.SingRep n Integer => SNat

instance TL.SingRep n Integer => SingI (n :: Nat) where 
  sing = SNat

instance SingKind ('KProxy :: KProxy Nat) where
  type DemoteRep ('KProxy :: KProxy Nat) = Integer
  fromSing (SNat :: Sing n) = TL.fromSing (TL.sing :: TL.Sing n)
  toSing n
    | n >= 0 = case TL.unsafeSingNat n of
                 (tlsing :: TL.Sing n) ->
                   withTLSingI tlsing (SomeSing (SNat :: Sing n))
    | otherwise = error "Negative singleton nat"

data instance Sing (n :: Symbol) = TL.SingRep n String => SSym

instance TL.SingRep n String => SingI (n :: Symbol) where
  sing = SSym

instance SingKind ('KProxy :: KProxy Symbol) where
  type DemoteRep ('KProxy :: KProxy Symbol) = String
  fromSing (SSym :: Sing n) = TL.fromSing (TL.sing :: TL.Sing n)
  toSing n = case TL.unsafeSingSymbol n of
               (tlsing :: TL.Sing n) ->
                 withTLSingI tlsing (SomeSing (SSym :: Sing n))

-- create 7.8-style TypeLits definitions:
class KnownNat (n :: Nat) where
  natVal :: proxy n -> Integer

class KnownSymbol (n :: Symbol) where
  symbolVal :: proxy n -> String

instance TL.SingI n => KnownNat n where
  natVal _ = TL.fromSing (TL.sing :: TL.Sing n)

instance TL.SingI n => KnownSymbol n where
  symbolVal _ = TL.fromSing (TL.sing :: TL.Sing n)

#endif

-- SDecide instances:
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
