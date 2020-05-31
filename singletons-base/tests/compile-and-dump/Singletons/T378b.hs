module T378b where

import Data.Kind
import Data.Singletons.Prelude.TH
import Singletons.Nat

$(singletons [d|
  type C :: forall b a. a -> b -> Constraint
  class C x y

  type D :: forall b a. a -> b -> Type
  data D x y

  f :: forall b a. a -> b -> ()
  f _ _ = ()

  natMinus :: Nat -> Nat -> Nat
  natMinus Zero       _        = Zero
  natMinus (Succ a)   (Succ b) = natMinus a b
  natMinus a@(Succ _) Zero     = a
  |])

-- Test some type variable orderings
type CExP :: Bool -> Ordering -> Constraint
type CExP = PC @Ordering @Bool

type CExS :: Bool -> Ordering -> Constraint
type CExS = SC @Ordering @Bool

type DExS :: D (x :: Bool) (y :: Ordering) -> Type
type DExS = SD @Ordering @Bool

type FEx0 :: Bool ~> Ordering ~> ()
type FEx0 = FSym0 @Ordering @Bool

type FEx1 :: Bool -> Ordering ~> ()
type FEx1 = FSym1 @Ordering @Bool

type FEx2 :: ()
type FEx2 = FSym2 @Ordering @Bool True EQ
