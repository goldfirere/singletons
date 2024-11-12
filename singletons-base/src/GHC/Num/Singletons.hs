{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Num.Singletons
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports promoted and singleton versions of definitions from
-- "GHC.Num".
--
-- Be warned that some of the associated type families in the 'PNum' class
-- (@(+)@, @(-)@, and @(*)@) clash with their counterparts for 'Natural' in the
-- "GHC.TypeLits" module.
----------------------------------------------------------------------------

module GHC.Num.Singletons (
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
import Data.Ord.Singletons
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH
import GHC.TypeLits.Singletons.Internal
import qualified GHC.TypeNats as TN
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
      -- Conversion from a 'Natural'.
      fromInteger         :: Natural -> a

      x - y               = x + negate y

      negate x            = 0 - x

  subtract :: Num a => a -> a -> a
  subtract x y = y - x

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
type SignumNat :: Natural -> Natural
type family SignumNat a where
  SignumNat 0 = 0
  SignumNat x = 1

instance PNum Natural where
  type a + b = a TN.+ b
  type a - b = a TN.- b
  type a * b = a TN.* b
  type Negate (a :: Natural) = Error "Cannot negate a natural number"
  type Abs (a :: Natural) = a
  type Signum a = SignumNat a
  type FromInteger a = a

-- SNum instance
instance SNum Natural where
  sa %+ sb =
    let a = fromSing sa
        b = fromSing sb
    in TN.withSomeSNat (a + b) unsafeCoerce

  sa %- sb =
    let a = fromSing sa
        b = fromSing sb
    in TN.withSomeSNat (a - b) unsafeCoerce

  sa %* sb =
    let a = fromSing sa
        b = fromSing sb
    in TN.withSomeSNat (a * b) unsafeCoerce

  sNegate _ = error "Cannot call sNegate on a natural number singleton."

  sAbs x = x

  sSignum sx =
    case sx %~ (sing :: Sing 0) of
      Proved Refl -> sing :: Sing 0
      Disproved _ -> unsafeCoerce (sing :: Sing 1)

  sFromInteger x = x
