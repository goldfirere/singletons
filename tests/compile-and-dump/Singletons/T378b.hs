module T378b where

import Data.Kind
import Data.Singletons.TH

$(singletons [d|
  type C :: forall b a. a -> b -> Constraint
  class C x y

  type D :: forall b a. a -> b -> Type
  data D x y

  f :: forall b a. a -> b -> ()
  f _ _ = ()

  type Nat :: Type
  data Nat = Z | S Nat

  -- This will only typecheck if ZSym0 is a type synonym.
  -- See Note [No SAKs for fully saturated defunctionalization symbols]
  -- in D.S.Promote.Defun for more information.
  natMinus :: Nat -> Nat -> Nat
  natMinus Z     _     = Z
  natMinus (S a) (S b) = natMinus a b
  natMinus a     Z     = a
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
