{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs, TypeOperators,
             DefaultSignatures, ScopedTypeVariables, InstanceSigs,
             MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, TypeInType #-}

module ByHand2 where

import Prelude hiding ( Eq(..), Ord(..), Bool(..), Ordering(..), not )
import Data.Kind (Type)
import Data.Singletons (Sing)

data Nat = Zero | Succ Nat
data Bool = False | True
data Ordering = LT | EQ | GT

not :: Bool -> Bool
not False = True
not True  = False

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  infix 4 ==, /=

  x == y = not (x /= y)
  x /= y = not (x == y)

instance Eq Nat where
  Zero == Zero = True
  Zero == Succ _ = False
  Succ _ == Zero = False
  Succ x == Succ y = x == y

data instance Sing :: Bool -> Type where
  SFalse :: Sing 'False
  STrue  :: Sing 'True

data instance Sing :: Nat -> Type where
  SZero :: Sing 'Zero
  SSucc :: Sing n -> Sing ('Succ n)

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

sNot :: Sing b -> Sing (Not b)
sNot STrue = SFalse
sNot SFalse = STrue

infix 4 :==, :/=
class PEq a where
  type (:==) (x :: a) (y :: a) :: Bool
  type (:/=) (x :: a) (y :: a) :: Bool

  type x :== y = Not (x :/= y)
  type x :/= y = Not (x :== y)

instance PEq Nat where
  type 'Zero :== 'Zero = 'True
  type 'Succ x :== 'Zero = 'False
  type 'Zero :== 'Succ x = 'False
  type 'Succ x :== 'Succ y = x :== y

class SEq a where
  (%:==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x :== y)
  (%:/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x :/= y)

  default (%:==) :: ((x :== y) ~ (Not (x :/= y))) => Sing (x :: a) -> Sing (y :: a) -> Sing (x :== y)
  x %:== y = sNot (x %:/= y)

  default (%:/=) :: ((x :/= y) ~ (Not (x :== y))) => Sing (x :: a) -> Sing (y :: a) -> Sing (x :/= y)
  x %:/= y = sNot (x %:== y)

instance SEq Nat where
  (%:==) :: forall (x :: Nat) (y :: Nat). Sing x -> Sing y -> Sing (x :== y)
  SZero %:== SZero = STrue
  SSucc _ %:== SZero = SFalse
  SZero %:== SSucc _ = SFalse
  SSucc x %:== SSucc y = x %:== y

instance Eq Ordering where
  LT == LT = True
  LT == EQ = False
  LT == GT = False
  EQ == LT = False
  EQ == EQ = True
  EQ == GT = False
  GT == LT = False
  GT == EQ = False
  GT == GT = True

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool

  x < y = compare x y == LT

class PEq a => POrd a where
  type Compare (x :: a) (y :: a) :: Ordering
  type (:<) (x :: a) (y :: a) :: Bool

  type x :< y = Compare x y :== 'LT

instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero (Succ _) = LT
  compare (Succ _) Zero = GT
  compare (Succ a) (Succ b) = compare a b

instance POrd Nat where
  type Compare 'Zero 'Zero = 'EQ
  type Compare 'Zero ('Succ x) = 'LT
  type Compare ('Succ x) 'Zero = 'GT
  type Compare ('Succ x) ('Succ y) = Compare x y

data instance Sing :: Ordering -> Type where
  SLT :: Sing 'LT
  SEQ :: Sing 'EQ
  SGT :: Sing 'GT

instance PEq Ordering where
  type 'LT :== 'LT = 'True
  type 'LT :== 'EQ = 'False
  type 'LT :== 'GT = 'False
  type 'EQ :== 'LT = 'False
  type 'EQ :== 'EQ = 'True
  type 'EQ :== 'GT = 'False
  type 'GT :== 'LT = 'False
  type 'GT :== 'EQ = 'False
  type 'GT :== 'GT = 'True

instance SEq Ordering where
  SLT %:== SLT = STrue
  SLT %:== SEQ = SFalse
  SLT %:== SGT = SFalse
  SEQ %:== SLT = SFalse
  SEQ %:== SEQ = STrue
  SEQ %:== SGT = SFalse
  SGT %:== SLT = SFalse
  SGT %:== SEQ = SFalse
  SGT %:== SGT = STrue

class SEq a => SOrd a where
  sCompare :: Sing (x :: a) -> Sing (y :: a) -> Sing (Compare x y)
  (%:<) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x :< y)

  default (%:<) :: ((x :< y) ~ (Compare x y :== 'LT)) => Sing (x :: a) -> Sing (y :: a) -> Sing (x :< y)
  x %:< y = sCompare x y %:== SLT

instance SOrd Nat where
  sCompare SZero SZero = SEQ
  sCompare SZero (SSucc _) = SLT
  sCompare (SSucc _) SZero = SGT
  sCompare (SSucc x) (SSucc y) = sCompare x y

class Pointed a where
  point :: a

class PPointed a where
  type Point :: a

class SPointed a where
  sPoint :: Sing (Point :: a)

instance Pointed Nat where
  point = Zero

instance PPointed Nat where
  type Point = 'Zero

instance SPointed Nat where
  sPoint = SZero

--------------------------------

class FD a b | a -> b where
  meth :: a -> a
  l2r  :: a -> b

instance FD Bool Nat where
  meth = not
  l2r False = Zero
  l2r True = Succ Zero

t1 = meth True
t2 = l2r False

class PFD a b | a -> b where
  type Meth (x :: a) :: a
  type L2r (x :: a) :: b

instance PFD Bool Nat where
  type Meth a = Not a
  type L2r 'False = 'Zero
  type L2r 'True = 'Succ 'Zero

type T1 = Meth 'True
type T2 = L2r 'False

class SFD a b | a -> b where
  sMeth :: forall (x :: a). Sing x -> Sing (Meth x :: a)
  sL2r :: forall (x :: a). Sing x -> Sing (L2r x :: b)

instance SFD Bool Nat where
  sMeth x = sNot x
  sL2r SFalse = SZero
  sL2r STrue = SSucc SZero

sT1 = sMeth STrue
sT2 :: Sing (T2 :: Nat)
sT2 = sL2r SFalse
