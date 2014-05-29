{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             TypeOperators, GADTs, ScopedTypeVariables, UndecidableInstances,
             DefaultSignatures, FlexibleContexts
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Num
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports promoted and singleton versions of definitions from
-- GHC.Num.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Num (
  PNum(..), SNum(..), Subtract, sSubtract,

  -- ** Defunctionalization symbols
  (:+$), (:+$$), (:+$$$),
  (:-$), (:-$$), (:-$$$),
  (:*$), (:*$$), (:*$$$),
  NegateSym0, NegateSym1,
  AbsSym0, AbsSym1,
  SignumSym0, SignumSym1,
  FromIntegerSym0, FromIntegerSym1,
  SubtractSym0, SubtractSym1, SubtractSym2
  ) where

import Data.Singletons.Promote
import Data.Singletons.Single
import Data.Singletons
import GHC.TypeLits

$(promoteOnly [d|
  -- | Basic numeric class.
  --
  -- Minimal complete definition: all except 'negate' or @(-)@
  class  Num a  where
      (+), (-), (*)       :: a -> a -> a
      -- | Unary negation.
      negate              :: a -> a
      -- | Absolute value.
      abs                 :: a -> a
      -- | Sign of a number.
      -- The functions 'abs' and 'signum' should satisfy the law:
      --
      -- > abs x * signum x == x
      --
      -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
      -- or @1@ (positive).
      signum              :: a -> a
      -- | Conversion from a 'Nat'.
      fromInteger         :: Nat -> a

      x - y               = x + negate y

      negate x            = 0 - x
  |])

class kproxy ~ 'KProxy => SNum (kproxy :: KProxy a) where
  (%:+) :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :+ y)
  (%:-) :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :- y)
  (%:*) :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :* y)
  sNegate :: forall (x :: a). Sing x -> Sing (Negate x)
  sAbs :: forall (x :: a). Sing x -> Sing (Abs x)
  sSignum :: forall (x :: a). Sing x -> Sing (Signum x)
  sFromInteger :: forall (n :: Nat). Sing n -> Sing (FromInteger n :: a)

  default (%:-) :: forall (x :: a) (y :: a).
                   ((x :- y) ~ (x :+ Negate y))
                => Sing x -> Sing y -> Sing (x :- y)
  x %:- y = x %:+ sNegate y

  default sNegate :: forall (x :: a).
                     (Negate x ~ (FromInteger 0 :- x), SingI 0)
                  => Sing x -> Sing (Negate x)
  sNegate x = sFromInteger (sing :: Sing 0) %:- x

$(singletonsOnly [d|
  subtract :: Num a => a -> a -> a
  subtract x y = y - x
  |])
