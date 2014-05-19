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

{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, FlexibleInstances,
             UndecidableInstances, ScopedTypeVariables, RankNTypes,
             GADTs, FlexibleContexts, TypeOperators, ConstraintKinds,
             TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Singletons.TypeLits (
  Nat, Symbol,
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, ErrorSym0, sError,
  KnownNat, natVal, KnownSymbol, symbolVal,

  (:+), (:-), (:*), (:^),
  (:+$), (:+$$), (:-$), (:-$$),
  (:*$), (:*$$), (:^$), (:^$$)
  ) where

import Data.Singletons
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Ord
import Data.Singletons.Decide
import Data.Singletons.Prelude.Bool
import Data.Singletons.Promote
import GHC.TypeLits as TL
import Data.Type.Equality
import Data.Proxy ( Proxy(..) )
import Unsafe.Coerce

----------------------------------------------------------------------
---- TypeLits singletons ---------------------------------------------
----------------------------------------------------------------------

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

-- Synonyms for GHC.TypeLits operations on Nat. These match our naming
-- conventions.
type x :+ y = x + y
type x :- y = x - y
type x :* y = x * y
type x :^ y = x ^ y

$(genDefunSymbols [ ''(:+), ''(:-), ''(:*), ''(:^)] )

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

-- PEq instances
instance PEq ('KProxy :: KProxy Nat) where
  type (a :: Nat) :== (b :: Nat) = a == b
instance PEq ('KProxy :: KProxy Symbol) where
  type (a :: Symbol) :== (b :: Symbol) = a == b

-- need SEq instances for TypeLits kinds
instance SEq ('KProxy :: KProxy Nat) where
  a %:== b
    | fromSing a == fromSing b    = unsafeCoerce STrue
    | otherwise                   = unsafeCoerce SFalse

instance SEq ('KProxy :: KProxy Symbol) where
  a %:== b
    | fromSing a == fromSing b    = unsafeCoerce STrue
    | otherwise                   = unsafeCoerce SFalse

-- POrd instances
instance POrd ('KProxy :: KProxy Nat) where
  type (a :: Nat) `Compare` (b :: Nat) = a `TL.CmpNat` b

instance POrd ('KProxy :: KProxy Symbol) where
  type (a :: Symbol) `Compare` (b :: Symbol) = a `TL.CmpSymbol` b

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
data ErrorSym0 (t1 :: TyFun k1 k2)
type instance Apply ErrorSym0 a = Error a

-- | The singleton for 'error'
sError :: Sing (str :: Symbol) -> a
sError sstr = error (fromSing sstr)
