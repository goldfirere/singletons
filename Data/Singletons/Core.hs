{- Data/Singletons/Core.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This (internal) module contains the main class definitions for singletons,
re-exported from various places.

-}

{-# LANGUAGE CPP, RankNTypes, DataKinds, PolyKinds, GADTs, TypeFamilies,
             FlexibleContexts, TemplateHaskell, ScopedTypeVariables,
             UndecidableInstances, TypeOperators, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE EmptyCase #-}
#endif
  -- optimizing instances of SDecide cause GHC to die (#8467)
{-# OPTIONS_GHC -O0 #-}

module Data.Singletons.Core where

import Data.Singletons.Singletons
import GHC.TypeLits (Nat, Symbol)
#if __GLASGOW_HASKELL__ < 707
import qualified GHC.TypeLits as TypeLits
#endif
import Data.Singletons.Types
import Unsafe.Coerce

#if __GLASGOW_HASKELL__ >= 707
import GHC.TypeLits (KnownNat, KnownSymbol, natVal, symbolVal)
#endif

-- Access the kind of a type variable
type KindOf (a :: k) = ('KProxy :: KProxy k)

-- Declarations of singleton structures
data family Sing (a :: k)
class SingI (a :: k) where
  sing :: Sing a
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam
  toSing   :: DemoteRep kparam -> SomeSing kparam

-- Abbreviation for DemoteRep
type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)

-- Wraps up a singleton
data SomeSing :: KProxy k -> * where
  SomeSing :: forall (a :: k). SingKind ('KProxy :: KProxy k)
           => Sing a -> SomeSing ('KProxy :: KProxy k)
                                  
-- some useful singletons
$(genSingletons [''Bool, ''Maybe, ''Either,  ''[]])
$(genSingletons [''(), ''(,), ''(,,), ''(,,,), ''(,,,,), ''(,,,,,), ''(,,,,,,)])

-- define singletons for TypeLits

newtype instance Sing (n :: Nat) = SNat Integer
#if __GLASGOW_HASKELL__ >= 707
instance KnownNat n => SingI n where
  sing = SNat (natVal (Proxy :: Proxy n))
#else
instance TypeLits.SingRep n Integer => SingI (n :: Nat) where
  sing = SNat (TypeLits.fromSing (TypeLits.sing :: TypeLits.Sing n))
#endif
instance SingKind ('KProxy :: KProxy Nat) where
  type DemoteRep ('KProxy :: KProxy Nat) = Integer
  fromSing (SNat n) = n
  toSing n = SomeSing (SNat n)

newtype instance Sing (n :: Symbol) = SSym String
#if __GLASGOW_HASKELL__ >= 707
instance KnownSymbol n => SingI n where
  sing = SSym (symbolVal (Proxy :: Proxy n))
#else
instance TypeLits.SingRep n String => SingI (n :: Symbol) where
  sing = SSym (TypeLits.fromSing (TypeLits.sing :: TypeLits.Sing n))
#endif
instance SingKind ('KProxy :: KProxy Symbol) where
  type DemoteRep ('KProxy :: KProxy Symbol) = String
  fromSing (SSym n) = n
  toSing s = SomeSing (SSym s)
  
-- we need to decare SDecide and its instances here to avoid making
-- the EqualityT instance an orphan

-- allows equality decisions over singletons
class (kparam ~ 'KProxy) => SDecide (kparam :: KProxy k) where
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

$(singDecideInstances [''Bool, ''Maybe, ''Either, ''[]])
$(singDecideInstances [''(), ''(,), ''(,,), ''(,,,), ''(,,,,), ''(,,,,,), ''(,,,,,,)])

-- We need SDecide instances for the TypeLits singletons
instance SDecide ('KProxy :: KProxy Nat) where
  (SNat n) %~ (SNat m)
    | n == m    = Proved $ unsafeCoerce Refl
    | otherwise = Disproved (\_ -> error errStr)
    where errStr = "Broken Nat singletons"
                  
instance SDecide ('KProxy :: KProxy Symbol) where
  (SSym n) %~ (SSym m)
    | n == m    = Proved $ unsafeCoerce Refl
    | otherwise = Disproved (\_ -> error errStr)
    where errStr = "Broken Symbol singletons"

instance SDecide ('KProxy :: KProxy k) => EqualityT (Sing :: k -> *) where
  equalsT a b =
    case a %~ b of
      Proved Refl -> Just Refl
      Disproved _ -> Nothing

