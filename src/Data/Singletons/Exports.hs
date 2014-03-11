{- Data/Singletons/Exports.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This module contains the main non-generated definitions for singletons.

-}

{-# LANGUAGE CPP, RankNTypes, DataKinds, PolyKinds, TypeOperators,
             TypeFamilies, GADTs, FlexibleInstances, FlexibleContexts,
             UndecidableInstances, ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ < 707
  -- optimizing instances of SDecide cause GHC to die (#8467)
{-# OPTIONS_GHC -O0 #-}
#endif
    
module Data.Singletons.Exports where

import Data.Singletons.Types
import GHC.TypeLits
import Unsafe.Coerce

-- | Convenient synonym to refer to the kind of a type variable:
-- @type KindOf (a :: k) = ('KProxy :: KProxy k)@
type KindOf (a :: k) = ('KProxy :: KProxy k)

----------------------------------------------------------------------
---- Sing & friends --------------------------------------------------
----------------------------------------------------------------------
                        
-- | The singleton kind-indexed data family.
data family Sing (a :: k)

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI'.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

-- | The 'SingKind' class is essentially a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  -- | Get a base type from a proxy for the promoted kind. For example,
  -- @DemoteRep ('KProxy :: KProxy Bool)@ will be the type @Bool@.
  type DemoteRep kparam :: *

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> DemoteRep kparam

  -- | Convert an unrefined type to an existentially-quantified singleton type.
  toSing   :: DemoteRep kparam -> SomeSing kparam

-- | Convenient abbreviation for 'DemoteRep':
-- @type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)@
type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)

-- | An /existentially-quantified/ singleton. This type is useful when you want a
-- singleton type, but there is no way of knowing, at compile-time, what the type
-- index will be. To make use of this type, you will generally have to use a
-- pattern-match:
--
-- > foo :: Bool -> ...
-- > foo b = case toSing b of
-- >           SomeSing sb -> {- fancy dependently-typed code with sb -}
--
-- An example like the one above may be easier to write using 'withSomeSing'.
data SomeSing (kproxy :: KProxy k) where
  SomeSing :: Sing (a :: k) -> SomeSing ('KProxy :: KProxy k)

----------------------------------------------------------------------
---- TypeLits singletons ---------------------------------------------
----------------------------------------------------------------------

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


----------------------------------------------------------------------
---- SDecide ---------------------------------------------------------
----------------------------------------------------------------------

-- we need to declare SDecide and its instances here to avoid making
-- the TestEquality instance an orphan

-- | Members of the 'SDecide' "kind" class support decidable equality. Instances
-- of this class are generated alongside singleton definitions for datatypes that
-- derive an 'Eq' instance.
class (kparam ~ 'KProxy) => SDecide (kparam :: KProxy k) where
  -- | Compute a proof or disproof of equality, given two singletons.
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

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

instance SDecide ('KProxy :: KProxy k) => TestEquality (Sing :: k -> *) where
  testEquality a b =
    case a %~ b of
      Proved Refl -> Just Refl
      Disproved _ -> Nothing

