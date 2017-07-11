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
             TypeInType, TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Singletons.TypeLits.Internal (
  Sing(..),

  Nat, Symbol,
  SNat, SSymbol, withKnownNat, withKnownSymbol,
  Error, sError,
  Undefined, sUndefined,
  KnownNat, natVal, KnownSymbol, symbolVal,
  type (^), (%^),
  type (<>), (%<>),

  -- * Defunctionalization symbols
  ErrorSym0, ErrorSym1, UndefinedSym0,
  type (^@#@$),  type (^@#@$$),  type (^@#@$$$),
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$)
  ) where

import Data.Singletons.Promote
import Data.Singletons.Internal
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Ord
import Data.Singletons.Decide
import Data.Singletons.Prelude.Bool
import GHC.TypeLits as TL
import qualified GHC.TypeNats as TN
import Data.Monoid ((<>))
import qualified Data.Type.Equality as DTE
import Data.Type.Equality ((:~:)(..))
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
    | TN.natVal (Proxy :: Proxy n) == TN.natVal (Proxy :: Proxy m)
    = Proved $ unsafeCoerce Refl
    | otherwise
    = Disproved (\_ -> error errStr)
    where errStr = "Broken Nat singletons"

instance SDecide Symbol where
  (SSym :: Sing n) %~ (SSym :: Sing m)
    | symbolVal (Proxy :: Proxy n) == symbolVal (Proxy :: Proxy m)
    = Proved $ unsafeCoerce Refl
    | otherwise
    = Disproved (\_ -> error errStr)
    where errStr = "Broken Symbol singletons"

-- PEq instances
instance PEq Nat where
  type (a :: Nat) == (b :: Nat) = a DTE.== b
instance PEq Symbol where
  type (a :: Symbol) == (b :: Symbol) = a DTE.== b

-- need SEq instances for TypeLits kinds
instance SEq Nat where
  a %== b
    | fromSing a == fromSing b    = unsafeCoerce STrue
    | otherwise                   = unsafeCoerce SFalse

instance SEq Symbol where
  a %== b
    | fromSing a == fromSing b    = unsafeCoerce STrue
    | otherwise                   = unsafeCoerce SFalse

-- POrd instances
instance POrd Nat where
  type (a :: Nat) `Compare` (b :: Nat) = a `TL.CmpNat` b

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

-- | The singleton for 'error'
sError :: Sing (str :: Symbol) -> a
sError sstr = error (T.unpack (fromSing sstr))

-- | The promotion of 'undefined'.
type family Undefined :: k where {}
$(genDefunSymbols [''Undefined])

-- | The singleton for 'undefined'.
sUndefined :: a
sUndefined = undefined

-- | The singleton analogue of '(TL.^)' for 'Nat's.
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

-- | The promoted analogue of '(<>)' for 'Symbol's. This uses the special
-- 'TL.AppendSymbol' type family from "GHC.TypeLits".
type a <> b = TL.AppendSymbol a b
infixr 6 <>

-- | The singleton analogue of '(<>)' for 'Symbol's.
(%<>) :: Sing a -> Sing b -> Sing (a <> b)
sa %<> sb =
    let a  = fromSing sa
        b  = fromSing sb
        ex = someSymbolVal $ T.unpack $ a <> b
    in case ex of
         SomeSymbol (_ :: Proxy ab) -> unsafeCoerce (SSym :: Sing ab)
infixr 6 %<>

$(genDefunSymbols [''(<>)])

------------------------------------------------------------
-- TypeLits singleton non-singleton instances
------------------------------------------------------------

-- Thanks to @cumber on #179

instance Show (SNat n) where
  showsPrec p n@SNat
    = showParen (p > atPrec)
      ( showString "SNat @"
        . showsPrec (atPrec + 1) (TN.natVal n)
      )
    where atPrec = 10

instance Show (SSymbol s) where
  showsPrec p s@SSym
    = showParen (p > atPrec)
      ( showString "SSym @"
        . showsPrec (atPrec + 1) (symbolVal s)
      )
    where atPrec = 10

deriving instance Show (SomeSing Nat)
deriving instance Show (SomeSing Symbol)
