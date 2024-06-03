{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs, TypeOperators,
             DefaultSignatures, ScopedTypeVariables, InstanceSigs,
             MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, CPP, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module ByHand2 where

import Data.Kind
import Data.Singletons (Sing)

#if __GLASGOW_HASKELL__ >= 810
type Nat :: Type
#endif
data Nat = Zero | Succ Nat

#if __GLASGOW_HASKELL__ >= 810
type SNat :: Nat -> Type
#endif
data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Nat =
#else
type instance Sing =
#endif
  SNat

{-
type Bool :: Type
data Bool = False | True
-}

#if __GLASGOW_HASKELL__ >= 810
type SBool :: Bool -> Type
#endif
data SBool :: Bool -> Type where
  SFalse :: SBool 'False
  STrue  :: SBool 'True
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Bool =
#else
type instance Sing =
#endif
  SBool

{-
type Ordering :: Type
data Ordering = LT | EQ | GT
-}

#if __GLASGOW_HASKELL__ >= 810
type SOrdering :: Ordering -> Type
#endif
data SOrdering :: Ordering -> Type where
  SLT :: SOrdering 'LT
  SEQ :: SOrdering 'EQ
  SGT :: SOrdering 'GT
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Ordering =
#else
type instance Sing =
#endif
  SOrdering

{-
not :: Bool -> Bool
not True  = False
not False = True
-}

#if __GLASGOW_HASKELL__ >= 810
type Not :: Bool -> Bool
#endif
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

sNot :: Sing b -> Sing (Not b)
sNot STrue = SFalse
sNot SFalse = STrue

{-
type Eq :: Type -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  infix 4 ==, /=

  x == y = not (x /= y)
  x /= y = not (x == y)
-}

#if __GLASGOW_HASKELL__ >= 810
type PEq :: Type -> Constraint
#endif
class PEq a where
  type (==) (x :: a) (y :: a) :: Bool
  type (/=) (x :: a) (y :: a) :: Bool

  type x == y = Not (x /= y)
  type x /= y = Not (x == y)

#if __GLASGOW_HASKELL__ >= 810
type SEq :: Type -> Constraint
#endif
class SEq a where
  (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
  (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)

  default (%==) :: ((x == y) ~ (Not (x /= y))) => Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
  x %== y = sNot (x %/= y)

  default (%/=) :: ((x /= y) ~ (Not (x == y))) => Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)
  x %/= y = sNot (x %== y)

instance Eq Nat where
  Zero == Zero = True
  Zero == Succ _ = False
  Succ _ == Zero = False
  Succ x == Succ y = x == y

instance PEq Nat where
  type 'Zero   == 'Zero   = 'True
  type 'Succ x == 'Zero   = 'False
  type 'Zero   == 'Succ x = 'False
  type 'Succ x == 'Succ y = x == y

instance SEq Nat where
  (%==) :: forall (x :: Nat) (y :: Nat). Sing x -> Sing y -> Sing (x == y)
  SZero   %== SZero   = STrue
  SSucc _ %== SZero   = SFalse
  SZero   %== SSucc _ = SFalse
  SSucc x %== SSucc y = x %== y

{-
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
-}

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

{-
type Ord :: Type -> Constraint
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool

  x < y = compare x y == LT
-}

#if __GLASGOW_HASKELL__ >= 810
type POrd :: Type -> Constraint
#endif
class PEq a => POrd a where
  type Compare (x :: a) (y :: a) :: Ordering
  type (<) (x :: a) (y :: a) :: Bool

  type x < y = Compare x y == 'LT

#if __GLASGOW_HASKELL__ >= 810
type SOrd :: Type -> Constraint
#endif
class SEq a => SOrd a where
  sCompare :: Sing (x :: a) -> Sing (y :: a) -> Sing (Compare x y)
  (%<) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x < y)

  default (%<) :: ((x < y) ~ (Compare x y == 'LT)) => Sing (x :: a) -> Sing (y :: a) -> Sing (x < y)
  x %< y = sCompare x y %== SLT

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

instance SOrd Nat where
  sCompare SZero SZero = SEQ
  sCompare SZero (SSucc _) = SLT
  sCompare (SSucc _) SZero = SGT
  sCompare (SSucc x) (SSucc y) = sCompare x y

#if __GLASGOW_HASKELL__ >= 810
type Pointed :: Type -> Constraint
#endif
class Pointed a where
  point :: a

#if __GLASGOW_HASKELL__ >= 810
type PPointed :: Type -> Constraint
#endif
class PPointed a where
  type Point :: a

#if __GLASGOW_HASKELL__ >= 810
type SPointed :: Type -> Constraint
#endif
class SPointed a where
  sPoint :: Sing (Point :: a)

instance Pointed Nat where
  point = Zero

instance PPointed Nat where
  type Point = 'Zero

instance SPointed Nat where
  sPoint = SZero

--------------------------------

#if __GLASGOW_HASKELL__ >= 810
type FD :: Type -> Type -> Constraint
#endif
class FD a b | a -> b where
  meth :: a -> a
  l2r  :: a -> b

instance FD Bool Nat where
  meth = not
  l2r False = Zero
  l2r True = Succ Zero

t1 = meth True
t2 = l2r False

#if __GLASGOW_HASKELL__ >= 810
type PFD :: Type -> Type -> Constraint
#endif
class PFD a b | a -> b where
  type Meth (x :: a) :: a
  type L2r (x :: a) :: b

instance PFD Bool Nat where
  type Meth a = Not a
  type L2r 'False = 'Zero
  type L2r 'True = 'Succ 'Zero

type T1 = Meth 'True

#if __GLASGOW_HASKELL__ >= 810
type T2 :: Nat
#endif
type T2 = (L2r 'False :: Nat)

#if __GLASGOW_HASKELL__ >= 810
type SFD :: Type -> Type -> Constraint
#endif
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
