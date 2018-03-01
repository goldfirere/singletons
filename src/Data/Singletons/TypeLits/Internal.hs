-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TypeLits.Internal
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports singletons useful for the Nat and Symbol kinds.
-- This exports the internal, unsafe constructors. Use Data.Singletons.TypeLits
-- for a safe interface.
--
----------------------------------------------------------------------------

{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, FlexibleInstances,
             UndecidableInstances, ScopedTypeVariables, RankNTypes,
             GADTs, FlexibleContexts, TypeOperators, ConstraintKinds,
             TypeInType, TemplateHaskell, StandaloneDeriving,
             TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Singletons.TypeLits.Internal (
  Sing(..),

  Nat, Symbol,
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, sError,
  ErrorWithoutStackTrace, sErrorWithoutStackTrace,
  Undefined, sUndefined,
  KnownNat, TN.natVal, KnownSymbol, symbolVal,
  type (^), (%^),
  type (<=?), (%<=?),

  -- * Defunctionalization symbols
  ErrorSym0, ErrorSym1,
  ErrorWithoutStackTraceSym0, ErrorWithoutStackTraceSym1,
  UndefinedSym0,
  type (^@#@$),  type (^@#@$$),  type (^@#@$$$),
  type (<=?@#@$),  type (<=?@#@$$),  type (<=?@#@$$$)
  ) where

import Data.Singletons.Promote
import Data.Singletons.Internal
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Ord as O
import Data.Singletons.Decide
import Data.Singletons.Prelude.Bool
import GHC.Stack (HasCallStack)
import GHC.TypeLits as TL
import qualified GHC.TypeNats as TN
import Data.Proxy ( Proxy(..) )
import Numeric.Natural (Natural)
import Unsafe.Coerce

import qualified Data.Text as T
import Data.Text ( Text )

----------------------------------------------------------------------
---- TypeLits singletons ---------------------------------------------
----------------------------------------------------------------------

data instance Sing (n :: Nat) = KnownNat n => SNat

instance KnownNat n => SingI n where
  sing = SNat

instance SingKind Nat where
  type Demote Nat = Natural
  fromSing (SNat :: Sing n) = TN.natVal (Proxy :: Proxy n)
  toSing n = case TN.someNatVal n of
               SomeNat (_ :: Proxy n) -> SomeSing (SNat :: Sing n)

data instance Sing (n :: Symbol) = KnownSymbol n => SSym

instance KnownSymbol n => SingI n where
  sing = SSym

instance SingKind Symbol where
  type Demote Symbol = Text
  fromSing (SSym :: Sing n) = T.pack (symbolVal (Proxy :: Proxy n))
  toSing s = case someSymbolVal (T.unpack s) of
               SomeSymbol (_ :: Proxy n) -> SomeSing (SSym :: Sing n)

-- SDecide instances:
instance SDecide Nat where
  (SNat :: Sing n) %~ (SNat :: Sing m)
    | Just r <- TN.sameNat (Proxy :: Proxy n) (Proxy :: Proxy m)
    = Proved r
    | otherwise
    = Disproved (\_ -> error errStr)
    where errStr = "Broken Nat singletons"

instance SDecide Symbol where
  (SSym :: Sing n) %~ (SSym :: Sing m)
    | Just r <- sameSymbol (Proxy :: Proxy n) (Proxy :: Proxy m)
    = Proved r
    | otherwise
    = Disproved (\_ -> error errStr)
    where errStr = "Broken Symbol singletons"

-- PEq instances
instance PEq Nat where
  type (a :: Nat) == (b :: Nat) = TL.CmpNat a b == 'EQ
instance PEq Symbol where
  type (a :: Symbol) == (b :: Symbol) = TL.CmpSymbol a b == 'EQ

-- need SEq instances for TypeLits kinds
instance SEq Nat where
  (SNat :: Sing n) %== (SNat :: Sing m)
    = case sameNat (Proxy :: Proxy n) (Proxy :: Proxy m) of
        Just Refl -> STrue
        Nothing   -> unsafeCoerce SFalse

instance SEq Symbol where
  (SSym :: Sing n) %== (SSym :: Sing m)
    = case sameSymbol (Proxy :: Proxy n) (Proxy :: Proxy m) of
        Just Refl -> STrue
        Nothing   -> unsafeCoerce SFalse

-- POrd instances
instance POrd Nat where
  type (a :: Nat) `Compare` (b :: Nat) = a `TN.CmpNat` b

instance POrd Symbol where
  type (a :: Symbol) `Compare` (b :: Symbol) = a `TL.CmpSymbol` b

-- | Kind-restricted synonym for 'Sing' for @Nat@s
type SNat (x :: Nat) = Sing x

-- | Kind-restricted synonym for 'Sing' for @Symbol@s
type SSymbol (x :: Symbol) = Sing x

-- SOrd instances
instance SOrd Nat where
  a `sCompare` b = case fromSing a `compare` fromSing b of
                     LT -> unsafeCoerce SLT
                     EQ -> unsafeCoerce SEQ
                     GT -> unsafeCoerce SGT

instance SOrd Symbol where
  a `sCompare` b = case fromSing a `compare` fromSing b of
                     LT -> unsafeCoerce SLT
                     EQ -> unsafeCoerce SEQ
                     GT -> unsafeCoerce SGT

-- Convenience functions

-- | Given a singleton for @Nat@, call something requiring a
-- @KnownNat@ instance.
withKnownNat :: Sing n -> (KnownNat n => r) -> r
withKnownNat SNat f = f

-- | Given a singleton for @Symbol@, call something requiring
-- a @KnownSymbol@ instance.
withKnownSymbol :: Sing n -> (KnownSymbol n => r) -> r
withKnownSymbol SSym f = f

-- | The promotion of 'error'. This version is more poly-kinded for
-- easier use.
type family Error (str :: k0) :: k where {}
$(genDefunSymbols [''Error])
instance SingI (ErrorSym0 :: Symbol ~> a) where
  sing = singFun1 sError

-- | The singleton for 'error'
sError :: HasCallStack => Sing (str :: Symbol) -> a
sError sstr = error (T.unpack (fromSing sstr))

-- | The promotion of 'errorWithoutStackTrace'. This version is more
-- poly-kinded for easier use.
type family ErrorWithoutStackTrace (str :: k0) :: k where {}
$(genDefunSymbols [''ErrorWithoutStackTrace])
instance SingI (ErrorWithoutStackTraceSym0 :: Symbol ~> a) where
  sing = singFun1 sErrorWithoutStackTrace

-- | The singleton for 'errorWithoutStackTrace'.
sErrorWithoutStackTrace :: Sing (str :: Symbol) -> a
sErrorWithoutStackTrace sstr = errorWithoutStackTrace (T.unpack (fromSing sstr))

-- | The promotion of 'undefined'.
type family Undefined :: k where {}
$(genDefunSymbols [''Undefined])

-- | The singleton for 'undefined'.
sUndefined :: HasCallStack => a
sUndefined = undefined

-- | The singleton analogue of '(TN.^)' for 'Nat's.
(%^) :: Sing a -> Sing b -> Sing (a ^ b)
sa %^ sb =
  let a = fromSing sa
      b = fromSing sb
      ex = TN.someNatVal (a ^ b)
  in
  case ex of
    SomeNat (_ :: Proxy ab) -> unsafeCoerce (SNat :: Sing ab)
infixr 8 %^

-- Defunctionalization symbols for type-level (^)
$(genDefunSymbols [''(^)])
instance SingI (^@#@$) where
  sing = singFun2 (%^)
instance SingI x => SingI ((^@#@$$) x) where
  sing = singFun1 (sing @_ @x %^)

-- | The singleton analogue of 'TN.<=?'
--
-- Note that, because of historical reasons in GHC's 'TN.Nat' API, 'TN.<=?'
-- is incompatible (unification-wise) with 'O.<=' and the 'PEq', 'SEq',
-- 'POrd', and 'SOrd' instances for 'Nat'.  @(a '<=?' b) ~ 'True@ does not
-- imply anything about @a 'O.<=' b@ or any other 'PEq' / 'POrd'
-- relationships.
--
-- (Be aware that 'O.<=' in the paragraph above refers to 'O.<=' from the
-- 'POrd' typeclass, exported from "Data.Singletons.Prelude.Ord", and /not/
-- the 'TN.<=' from "GHC.TypeNats".  The latter is simply a type alias for
-- @(a 'TN.<=?' b) ~ 'True@.)
--
-- This is provided here for the sake of completeness and for compatibility
-- with libraries with APIs built around '<=?'.  New code should use
-- 'CmpNat', exposed through this library through the 'POrd' and 'SOrd'
-- instances for 'Nat'.
(%<=?) :: Sing a -> Sing b -> Sing (a <=? b)
sa %<=? sb = unsafeCoerce (sa %<= sb)
infix 4 %<=?

-- Defunctionalization symbols for (<=?)
$(genDefunSymbols [''(<=?)])
instance SingI (<=?@#@$) where
  sing = singFun2 (%<=?)
instance SingI x => SingI ((<=?@#@$$) x) where
  sing = singFun1 (sing @_ @x %<=?)
