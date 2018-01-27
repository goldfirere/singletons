{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, TypeInType,
             TypeOperators, GADTs, ScopedTypeVariables, UndecidableInstances,
             DefaultSignatures, FlexibleContexts, InstanceSigs
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Num
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports promoted and singleton versions of definitions from
-- GHC.Num.
--
-- Be warned that some of the associated type families in the 'PNum' class
-- (@(+)@, @(-)@, and @(*)@) clash with their counterparts for 'Nat' in the
-- "GHC.TypeLits" module.
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Num (
  PNum(..), SNum(..), Subtract, sSubtract,

  -- ** Defunctionalization symbols
  type (+@#@$), type (+@#@$$), type (+@#@$$$),
  type (-@#@$), type (-@#@$$), type (-@#@$$$),
  type (*@#@$), type (*@#@$$), type (*@#@$$$),
  NegateSym0, NegateSym1,
  AbsSym0, AbsSym1,
  SignumSym0, SignumSym1,
  FromIntegerSym0, FromIntegerSym1,
  SubtractSym0, SubtractSym1, SubtractSym2
  ) where

import Data.Ord (Down(..))
import Data.Singletons.Single
import Data.Singletons.Internal
import Data.Singletons.Prelude.Ord
import Data.Singletons.TypeLits.Internal
import Data.Singletons.Decide
import qualified GHC.TypeNats as TN
import GHC.TypeNats (Nat, SomeNat(..), someNatVal)
import Unsafe.Coerce

$(singletonsOnly [d|
  -- Basic numeric class.
  --
  -- Minimal complete definition: all except 'negate' or @(-)@
  class  Num a  where
      (+), (-), (*)       :: a -> a -> a
      infixl 6 +
      infixl 6 -
      infixl 7 *
      -- Unary negation.
      negate              :: a -> a
      -- Absolute value.
      abs                 :: a -> a
      -- Sign of a number.
      -- The functions 'abs' and 'signum' should satisfy the law:
      --
      -- > abs x * signum x == x
      --
      -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
      -- or @1@ (positive).
      signum              :: a -> a
      -- Conversion from a 'Nat'.
      fromInteger         :: Nat -> a

      x - y               = x + negate y

      negate x            = 0 - x

  -- deriving newtype instance Num a => Num (Down a)
  instance Num a => Num (Down a) where
      Down a + Down b = Down (a + b)
      Down a - Down b = Down (a - b)
      Down a * Down b = Down (a * b)
      negate (Down a) = Down (negate a)
      abs    (Down a) = Down (abs a)
      signum (Down a) = Down (signum a)
      fromInteger n   = Down (fromInteger n)
  |])

-- PNum instance
type family SignumNat (a :: Nat) :: Nat where
  SignumNat 0 = 0
  SignumNat x = 1

instance PNum Nat where
  type a + b = a TN.+ b
  type a - b = a TN.- b
  type a * b = a TN.* b
  type Negate (a :: Nat) = Error "Cannot negate a natural number"
  type Abs (a :: Nat) = a
  type Signum a = SignumNat a
  type FromInteger a = a

-- SNum instance
instance SNum Nat where
  sa %+ sb =
    let a = fromSing sa
        b = fromSing sb
        ex = someNatVal (a + b)
    in
    case ex of
      SomeNat (_ :: Proxy ab) -> unsafeCoerce (SNat :: Sing ab)

  sa %- sb =
    let a = fromSing sa
        b = fromSing sb
        ex = someNatVal (a - b)
    in
    case ex of
      SomeNat (_ :: Proxy ab) -> unsafeCoerce (SNat :: Sing ab)

  sa %* sb =
    let a = fromSing sa
        b = fromSing sb
        ex = someNatVal (a * b)
    in
    case ex of
      SomeNat (_ :: Proxy ab) -> unsafeCoerce (SNat :: Sing ab)

  sNegate _ = error "Cannot call sNegate on a natural number singleton."

  sAbs x = x

  sSignum sx =
    case sx %~ (sing :: Sing 0) of
      Proved Refl -> sing :: Sing 0
      Disproved _ -> unsafeCoerce (sing :: Sing 1)

  sFromInteger x = x

$(singletonsOnly [d|
  subtract :: Num a => a -> a -> a
  subtract x y = y - x
  |])
