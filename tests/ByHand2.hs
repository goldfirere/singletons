{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs, TypeOperators,
             DefaultSignatures, ScopedTypeVariables, InstanceSigs,
             MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module ByHand2 where

import Prelude hiding ( Eq(..), Ord(..), Bool(..), Ordering(..), not )
import Data.Kind
import Data.Singletons (Sing)

type Nat :: Type
data Nat = Zero | Succ Nat

type Bool :: Type
data Bool = False | True

type Ordering :: Type
data Ordering = LT | EQ | GT

not :: Bool -> Bool
not False = True
not True  = False

type Eq :: Type -> Constraint
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

type SBool :: Bool -> Type
data SBool b where
  SFalse :: SBool 'False
  STrue  :: SBool 'True
type instance Sing = SBool

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)
type instance Sing = SNat

type Not :: Bool -> Bool
type family Not x where
  Not 'True = 'False
  Not 'False = 'True

sNot :: Sing b -> Sing (Not b)
sNot STrue = SFalse
sNot SFalse = STrue

type PEq :: Type -> Constraint
class PEq a where
  type (==) (x :: a) (y :: a) :: Bool
  type (/=) (x :: a) (y :: a) :: Bool

  type x == y = Not (x /= y)
  type x /= y = Not (x == y)

instance PEq Nat where
  type 'Zero   == 'Zero   = 'True
  type 'Succ x == 'Zero   = 'False
  type 'Zero   == 'Succ x = 'False
  type 'Succ x == 'Succ y = x == y

type SEq :: Type -> Constraint
class SEq a where
  (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
  (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)

  default (%==) :: ((x == y) ~ (Not (x /= y))) => Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
  x %== y = sNot (x %/= y)

  default (%/=) :: ((x /= y) ~ (Not (x == y))) => Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)
  x %/= y = sNot (x %== y)

instance SEq Nat where
  (%==) :: forall (x :: Nat) (y :: Nat). Sing x -> Sing y -> Sing (x == y)
  SZero   %== SZero   = STrue
  SSucc _ %== SZero   = SFalse
  SZero   %== SSucc _ = SFalse
  SSucc x %== SSucc y = x %== y

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

type Ord :: Type -> Constraint
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool

  x < y = compare x y == LT

type POrd :: Type -> Constraint
class PEq a => POrd a where
  type Compare (x :: a) (y :: a) :: Ordering
  type (<) (x :: a) (y :: a) :: Bool

  type x < y = Compare x y == 'LT

instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero (Succ _) = LT
  compare (Succ _) Zero = GT
  compare (Succ a) (Succ b) = compare a b

instance POrd Nat where
  type Compare 'Zero     'Zero     = 'EQ
  type Compare 'Zero     ('Succ x) = 'LT
  type Compare ('Succ x) 'Zero     = 'GT
  type Compare ('Succ x) ('Succ y) = Compare x y

type SOrdering :: Ordering -> Type
data SOrdering o where
  SLT :: SOrdering 'LT
  SEQ :: SOrdering 'EQ
  SGT :: SOrdering 'GT
type instance Sing = SOrdering

instance PEq Ordering where
  type 'LT == 'LT = 'True
  type 'LT == 'EQ = 'False
  type 'LT == 'GT = 'False
  type 'EQ == 'LT = 'False
  type 'EQ == 'EQ = 'True
  type 'EQ == 'GT = 'False
  type 'GT == 'LT = 'False
  type 'GT == 'EQ = 'False
  type 'GT == 'GT = 'True

instance SEq Ordering where
  SLT %== SLT = STrue
  SLT %== SEQ = SFalse
  SLT %== SGT = SFalse
  SEQ %== SLT = SFalse
  SEQ %== SEQ = STrue
  SEQ %== SGT = SFalse
  SGT %== SLT = SFalse
  SGT %== SEQ = SFalse
  SGT %== SGT = STrue

type SOrd :: Type -> Constraint
class SEq a => SOrd a where
  sCompare :: Sing (x :: a) -> Sing (y :: a) -> Sing (Compare x y)
  (%<) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x < y)

  default (%<) :: ((x < y) ~ (Compare x y == 'LT)) => Sing (x :: a) -> Sing (y :: a) -> Sing (x < y)
  x %< y = sCompare x y %== SLT

instance SOrd Nat where
  sCompare SZero SZero = SEQ
  sCompare SZero (SSucc _) = SLT
  sCompare (SSucc _) SZero = SGT
  sCompare (SSucc x) (SSucc y) = sCompare x y

type Pointed :: Type -> Constraint
class Pointed a where
  point :: a

type PPointed :: Type -> Constraint
class PPointed a where
  type Point :: a

type SPointed :: Type -> Constraint
class SPointed a where
  sPoint :: Sing (Point :: a)

instance Pointed Nat where
  point = Zero

instance PPointed Nat where
  type Point = 'Zero

instance SPointed Nat where
  sPoint = SZero

--------------------------------

type FD :: Type -> Type -> Constraint
class FD a b | a -> b where
  meth :: a -> a
  l2r  :: a -> b

instance FD Bool Nat where
  meth = not
  l2r False = Zero
  l2r True = Succ Zero

t1 = meth True
t2 = l2r False

type PFD :: Type -> Type -> Constraint
class PFD a b | a -> b where
  type Meth (x :: a) :: a
  type L2r (x :: a) :: b

instance PFD Bool Nat where
  type Meth a = Not a
  type L2r 'False = 'Zero
  type L2r 'True = 'Succ 'Zero

type T1 :: Bool
type T1 = Meth 'True

type T2 :: Nat
type T2 = L2r 'False

type SFD :: Type -> Type -> Constraint
class SFD a b | a -> b where
  sMeth :: forall (x :: a). Sing x -> Sing (Meth x :: a)
  sL2r :: forall (x :: a). Sing x -> Sing (L2r x :: b)

instance SFD Bool Nat where
  sMeth x = sNot x
  sL2r SFalse = SZero
  sL2r STrue = SSucc SZero

sT1 = sMeth STrue
sT2 :: Sing T2
sT2 = sL2r SFalse
