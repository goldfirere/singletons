{- ByHand.hs

(c) Richard Eisenberg 2012
rae@cs.brynmawr.edu

Shows the derivations for the singleton definitions done by hand.
This file is a great way to understand the singleton encoding better.

-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-orphans #-}

{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, KindSignatures, GADTs,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             RankNTypes, TypeOperators, MultiParamTypeClasses,
             FunctionalDependencies, ScopedTypeVariables,
             LambdaCase, EmptyCase,
             TypeApplications, EmptyCase, CPP #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module ByHand where

import Data.Kind
import Data.Type.Equality hiding (type (==), apply)
import Data.Proxy
import Data.Singletons
import Data.Singletons.Decide
import Prelude hiding ((+), (-), map, zipWith)
import Unsafe.Coerce

-----------------------------------
-- Original ADTs ------------------
-----------------------------------

#if __GLASGOW_HASKELL__ >= 810
type Nat :: Type
#endif
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Eq

-- Defined using names to avoid fighting with concrete syntax
#if __GLASGOW_HASKELL__ >= 810
type List :: Type -> Type
#endif
data List :: Type -> Type where
  Nil :: List a
  Cons :: a -> List a -> List a
  deriving Eq

-----------------------------------
-- One-time definitions -----------
-----------------------------------

-- Promoted equality type class
#if __GLASGOW_HASKELL__ >= 810
type PEq :: Type -> Constraint
#endif
class PEq k where
  type (==) (a :: k) (b :: k) :: Bool
  -- omitting definition of /=

-- Singleton type equality type class
#if __GLASGOW_HASKELL__ >= 810
type SEq :: Type -> Constraint
#endif
class SEq k where
  (%==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a == b)
  -- omitting definition of %/=

#if __GLASGOW_HASKELL__ >= 810
type If :: Bool -> a -> a -> a
#endif
type family If (cond :: Bool) (tru :: a) (fls :: a) :: a where
  If True  tru  fls = tru
  If False tru  fls = fls

sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

-----------------------------------
-- Auto-generated code ------------
-----------------------------------

-- Nat

#if __GLASGOW_HASKELL__ >= 810
type SNat :: Nat -> Type
#endif
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Nat =
#else
type instance Sing =
#endif
  SNat

#if _
_GLASGOW_HASKELL__ >= 810
type SuccSym0 :: Nat ~> Nat
#endif
data SuccSym0 :: Nat ~> Nat
type instance Apply SuccSym0 x = Succ x

#if __GLASGOW_HASKELL__ >= 810
type EqualsNat :: Nat -> Nat -> Bool
#endif
type family EqualsNat (a :: Nat) (b :: Nat) :: Bool where
  EqualsNat Zero Zero = True
  EqualsNat (Succ a) (Succ b) = a == b
  EqualsNat (n1 :: Nat) (n2 :: Nat) = False
instance PEq Nat where
  type a == b = EqualsNat a b

instance SEq Nat where
  SZero %== SZero = STrue
  SZero %== (SSucc _) = SFalse
  (SSucc _) %== SZero = SFalse
  (SSucc n) %== (SSucc n') = n %== n'

instance SDecide Nat where
  SZero %~ SZero = Proved Refl
  (SSucc m) %~ (SSucc n) =
    case m %~ n of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  SZero %~ (SSucc _) = Disproved (\case)
  (SSucc _) %~ SZero = Disproved (\case)

instance SingI Zero where
  sing = SZero
instance SingI n => SingI (Succ n) where
  sing = SSucc sing
instance SingI1 Succ where
  liftSing = SSucc

type instance Demote Nat = Nat
type instance Promote Nat = Nat

type instance DemoteX Zero = Zero
type instance DemoteX (Succ n) = Succ (DemoteX n)

type instance PromoteX Zero = Zero
type instance PromoteX (Succ n) = Succ (PromoteX n)

type instance SingKindC Zero = ()
type instance SingKindC (Succ n) = SingKindC n

instance SingKind Nat where
  fromSing SZero = Zero
  fromSing (SSucc n) = Succ (fromSing n)
  toSing Zero = SomeSing SZero
  toSing (Succ n) = withSomeSing n (\n' -> SomeSing $ SSucc n')

-- Bool

#if __GLASGOW_HASKELL__ >= 810
type SBool :: Bool -> Type
#endif
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue :: SBool True
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Bool =
#else
type instance Sing =
#endif
  SBool

{-
(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && x = x
-}

#if __GLASGOW_HASKELL__ >= 810
type (&&) :: Bool -> Bool -> Bool
#endif
type family (a :: Bool) && (b :: Bool) :: Bool where
  False && _ = False
  True  && x = x

(%&&) :: forall (a :: Bool) (b :: Bool). Sing a -> Sing b -> Sing (a && b)
SFalse %&& SFalse = SFalse
SFalse %&& STrue = SFalse
STrue %&& SFalse = SFalse
STrue %&& STrue = STrue

instance SingI False where
  sing = SFalse
instance SingI True where
  sing = STrue

type instance Demote Bool = Bool
type instance Promote Bool = Bool

type instance DemoteX False = False
type instance DemoteX True = True

type instance PromoteX False = False
type instance PromoteX True = True

type instance SingKindC False = ()
type instance SingKindC True = ()

instance SingKind Bool where
  fromSing SFalse = False
  fromSing STrue = True
  toSing False = SomeSing SFalse
  toSing True  = SomeSing STrue

-- Maybe

#if __GLASGOW_HASKELL__ >= 810
type SMaybe :: forall k. Maybe k -> Type
#endif
data SMaybe :: forall k. Maybe k -> Type where
  SNothing :: SMaybe Nothing
  SJust :: forall k (a :: k). Sing a -> SMaybe (Just a)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @(Maybe k) =
#else
type instance Sing =
#endif
  SMaybe

#if __GLASGOW_HASKELL__ >= 810
type EqualsMaybe :: Maybe k -> Maybe k -> Bool
#endif
type family EqualsMaybe (a :: Maybe k) (b :: Maybe k) :: Bool where
  EqualsMaybe Nothing Nothing = True
  EqualsMaybe (Just a) (Just a') = a == a'
  EqualsMaybe (x :: Maybe k) (y :: Maybe k) = False
instance PEq a => PEq (Maybe a) where
  type m1 == m2 = EqualsMaybe m1 m2

instance SDecide k => SDecide (Maybe k) where
  SNothing %~ SNothing = Proved Refl
  (SJust x) %~ (SJust y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  SNothing %~ (SJust _) = Disproved (\case)
  (SJust _) %~ SNothing = Disproved (\case)

instance SEq k => SEq (Maybe k) where
  SNothing %== SNothing = STrue
  SNothing %== (SJust _) = SFalse
  (SJust _) %== SNothing = SFalse
  (SJust a) %== (SJust a') = a %== a'

instance SingI (Nothing :: Maybe k) where
  sing = SNothing
instance SingI a => SingI (Just (a :: k)) where
  sing = SJust sing
instance SingI1 Just where
  liftSing = SJust

type instance Demote (Maybe k) = Maybe (DemoteX k)
type instance Promote (Maybe k) = Maybe (PromoteX k)

type instance DemoteX Nothing = Nothing
type instance DemoteX (Just x) = Just (DemoteX x)

type instance PromoteX Nothing = Nothing
type instance PromoteX (Just x) = Just (PromoteX x)

type instance SingKindC Nothing = ()
type instance SingKindC (Just x) = SingKindC x

instance SingKind k => SingKind (Maybe k) where
  fromSing SNothing = Nothing
  fromSing (SJust a) = Just (fromSing a)
  toSing Nothing = SomeSing SNothing
  toSing (Just x) =
    case toSing x :: SomeSing k of
      SomeSing x' -> SomeSing $ SJust x'

-- List

#if __GLASGOW_HASKELL__ >= 810
type SList :: forall k. List k -> Type
#endif
data SList :: forall k. List k -> Type where
  SNil :: SList Nil
  SCons :: forall k (h :: k) (t :: List k). Sing h -> SList t -> SList (Cons h t)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @(List k) =
#else
type instance Sing =
#endif
  SList

#if __GLASGOW_HASKELL__ >= 810
type NilSym0 :: List a
#endif
type family NilSym0 :: List a where
  NilSym0 = Nil

#if __GLASGOW_HASKELL__ >= 810
type ConsSym0 :: forall a. a ~> List a ~> List a
type ConsSym1 :: forall a. a -> List a ~> List a
type ConsSym2 :: forall a. a -> List a -> List a
#endif
data ConsSym0 :: forall a. a ~> List a ~> List a
data ConsSym1 :: forall a. a -> List a ~> List a
type family ConsSym2 (x :: a) (y :: List a) :: List a where
  ConsSym2 x y = Cons x y
type instance Apply ConsSym0 a = ConsSym1 a
type instance Apply (ConsSym1 a) b = Cons a b

#if __GLASGOW_HASKELL__ >= 810
type EqualsList :: List k -> List k -> Bool
#endif
type family EqualsList (a :: List k) (b :: List k) :: Bool where
  EqualsList Nil Nil = True
  EqualsList (Cons a b) (Cons a' b') = (a == a') && (b == b')
  EqualsList (x :: List k) (y :: List k) = False
instance PEq a => PEq (List a) where
  type l1 == l2 = EqualsList l1 l2

instance SEq k => SEq (List k) where
  SNil %== SNil = STrue
  SNil %== (SCons _ _) = SFalse
  (SCons _ _) %== SNil = SFalse
  (SCons a b) %== (SCons a' b') = (a %== a') %&& (b %== b')

instance SDecide k => SDecide (List k) where
  SNil %~ SNil = Proved Refl
  (SCons h1 t1) %~ (SCons h2 t2) =
    case (h1 %~ h2, t1 %~ t2) of
      (Proved Refl, Proved Refl) -> Proved Refl
      (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
      (_, Disproved contra) -> Disproved (\Refl -> contra Refl)
  SNil %~ (SCons _ _) = Disproved (\case)
  (SCons _ _) %~ SNil = Disproved (\case)

instance SingI Nil where
  sing = SNil
instance (SingI h, SingI t) =>
           SingI (Cons (h :: k) (t :: List k)) where
  sing = SCons sing sing
instance SingI h => SingI1 (Cons (h :: k)) where
  liftSing = SCons sing
instance SingI2 Cons where
  liftSing2 = SCons

type instance Demote (List k) = List (DemoteX k)
type instance Promote (List k) = List (PromoteX k)

type instance DemoteX Nil = Nil
type instance DemoteX (Cons x xs) = Cons (DemoteX x) (DemoteX xs)

type instance PromoteX Nil = Nil
type instance PromoteX (Cons x xs) = Cons (PromoteX x) (PromoteX xs)

type instance SingKindC Nil = ()
type instance SingKindC (Cons x xs) = (SingKindC x, SingKindC xs)

instance SingKind k => SingKind (List k) where
  fromSing SNil = Nil
  fromSing (SCons h t) = Cons (fromSing h) (fromSing t)
  toSing Nil = SomeSing SNil
  toSing (Cons h t) =
    case ( toSing h :: SomeSing k
         , toSing t :: SomeSing (List k) ) of
      (SomeSing h', SomeSing t') -> SomeSing $ SCons h' t'

-- Either

#if __GLASGOW_HASKELL__ >= 810
type SEither :: forall k1 k2. Either k1 k2 -> Type
#endif
data SEither :: forall k1 k2. Either k1 k2 -> Type where
  SLeft :: forall k1 (a :: k1). Sing a -> SEither (Left a)
  SRight :: forall k2 (b :: k2). Sing b -> SEither (Right b)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @(Either k1 k2) =
#else
type instance Sing =
#endif
  SEither

instance (SingI a) => SingI (Left (a :: k)) where
  sing = SLeft sing
instance SingI1 Left where
  liftSing = SLeft
instance (SingI b) => SingI (Right (b :: k)) where
  sing = SRight sing
instance SingI1 Right where
  liftSing = SRight

type instance Demote (Either k1 k2) = Either (DemoteX k1) (DemoteX k2)
type instance Promote (Either k1 k2) = Either (PromoteX k1) (PromoteX k2)

type instance DemoteX (Left x) = Left (DemoteX x)
type instance DemoteX (Right y) = Right (DemoteX y)

type instance PromoteX (Left x) = Left (PromoteX x)
type instance PromoteX (Right y) = Right (PromoteX y)

type instance SingKindC (Left x) = SingKindC x
type instance SingKindC (Right y) = SingKindC y

instance (SingKind k1, SingKind k2) => SingKind (Either k1 k2) where
  fromSing (SLeft x) = Left (fromSing x)
  fromSing (SRight x) = Right (fromSing x)
  toSing (Left x) =
    case toSing x :: SomeSing k1 of
      SomeSing x' -> SomeSing $ SLeft x'
  toSing (Right x) =
    case toSing x :: SomeSing k2 of
      SomeSing x' -> SomeSing $ SRight x'

instance (SDecide k1, SDecide k2) => SDecide (Either k1 k2) where
  (SLeft x) %~ (SLeft y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SRight x) %~ (SRight y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SLeft _) %~ (SRight _) = Disproved (\case)
  (SRight _) %~ (SLeft _) = Disproved (\case)

-- Composite

#if __GLASGOW_HASKELL__ >= 810
type Composite :: Type -> Type -> Type
#endif
data Composite :: Type -> Type -> Type where
  MkComp :: Either (Maybe a) b -> Composite a b

#if __GLASGOW_HASKELL__ >= 810
type SComposite :: forall k1 k2. Composite k1 k2 -> Type
#endif
data SComposite :: forall k1 k2. Composite k1 k2 -> Type where
  SMkComp :: forall k1 k2 (a :: Either (Maybe k1) k2). SEither a -> SComposite (MkComp a)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @(Composite k1 k2) =
#else
type instance Sing =
#endif
  SComposite

instance SingI a => SingI (MkComp (a :: Either (Maybe k1) k2)) where
  sing = SMkComp sing
instance SingI1 MkComp where
  liftSing = SMkComp

type instance Demote (Composite k1 k2) = Composite (DemoteX k1) (DemoteX k2)
type instance Promote (Composite k1 k2) = Composite (PromoteX k1) (PromoteX k2)

type instance DemoteX (MkComp x) = MkComp (DemoteX x)
type instance PromoteX (MkComp x) = MkComp (PromoteX x)
type instance SingKindC (MkComp x) = SingKindC x

instance (SingKind k1, SingKind k2) => SingKind (Composite k1 k2) where
  fromSing (SMkComp x) = MkComp (fromSing x)
  toSing (MkComp x) =
    case toSing x :: SomeSing (Either (Maybe k1) k2) of
      SomeSing x' -> SomeSing $ SMkComp x'

instance (SDecide k1, SDecide k2) => SDecide (Composite k1 k2) where
  (SMkComp x) %~ (SMkComp y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)

-- Empty

#if __GLASGOW_HASKELL__ >= 810
type Empty :: Type
#endif
data Empty

#if __GLASGOW_HASKELL__ >= 810
type SEmpty :: Empty -> Type
#endif
data SEmpty :: Empty -> Type

#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Empty =
#else
type instance Sing =
#endif
  SEmpty

type instance Demote Empty = Empty
type instance Promote Empty = Empty

instance SingKind Empty where
  fromSing = \case
  toSing x = SomeSing (case x of)

-- Type

#if __GLASGOW_HASKELL__ >= 810
type Vec :: Type -> Nat -> Type
#endif
data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

#if __GLASGOW_HASKELL__ >= 810
type Rep :: Type
#endif
data Rep = Nat | Maybe Rep | Vec Rep Nat

#if __GLASGOW_HASKELL__ >= 810
type SRep :: Type -> Type
#endif
data SRep :: Type -> Type where
  SNat :: SRep Nat
  SMaybe :: SRep a -> SRep (Maybe a)
  SVec :: SRep a -> SNat n -> SRep (Vec a n)
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @Type =
#else
type instance Sing =
#endif
  SRep

instance SingI Nat where
  sing = SNat
instance SingI a => SingI (Maybe a) where
  sing = SMaybe sing
instance SingI1 Maybe where
  liftSing = SMaybe
instance (SingI a, SingI n) => SingI (Vec a n) where
  sing = SVec sing sing
instance SingI a => SingI1 (Vec a) where
  liftSing = SVec sing
instance SingI2 Vec where
  liftSing2 = SVec

{-
-- TODO RGS: What should we do about this?

instance SingKind Type where
  fromSing SNat = Nat
  fromSing (SMaybe a) = Maybe (fromSing a)
  fromSing (SVec a n) = Vec (fromSing a) (fromSing n)

  toSing Nat = SomeSing SNat
  toSing (Maybe a) =
    case toSing a :: SomeSing Type of
      SomeSing a' -> SomeSing $ SMaybe a'
  toSing (Vec a n) =
    case ( toSing a :: SomeSing Type
         , toSing n :: SomeSing Nat) of
      (SomeSing a', SomeSing n') -> SomeSing $ SVec a' n'
-}

instance SDecide Type where
  SNat %~ SNat = Proved Refl
  SNat %~ (SMaybe {}) = Disproved (\case)
  SNat %~ (SVec {}) = Disproved (\case)
  (SMaybe {}) %~ SNat = Disproved (\case)
  (SMaybe a) %~ (SMaybe b) =
    case a %~ b of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SMaybe {}) %~ (SVec {}) = Disproved (\case)
  (SVec {}) %~ SNat = Disproved (\case)
  (SVec {}) %~ (SMaybe {}) = Disproved (\case)
  (SVec a1 n1) %~ (SVec a2 n2) =
    case (a1 %~ a2, n1 %~ n2) of
      (Proved Refl, Proved Refl) -> Proved Refl
      (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
      (_, Disproved contra) -> Disproved (\Refl -> contra Refl)

#if __GLASGOW_HASKELL__ >= 810
type EqualsType :: Type -> Type -> Bool
#endif
type family EqualsType (a :: Type) (b :: Type) :: Bool where
  EqualsType a a = True
  EqualsType _ _ = False
instance PEq Type where
  type a == b = EqualsType a b

instance SEq Type where
  a %== b =
    case a %~ b of
      Proved Refl -> STrue
      Disproved _ -> unsafeCoerce SFalse

-----------------------------------
-- Some example functions ---------
-----------------------------------

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

#if __GLASGOW_HASKELL__ >= 810
type IsJust :: Maybe k -> Bool
#endif
type family IsJust (a :: Maybe k) :: Bool where
    IsJust Nothing = False
    IsJust (Just a) = True

-- defunctionalization symbols
#if __GLASGOW_HASKELL__ >= 810
type IsJustSym0 :: forall a. Maybe a ~> Bool
#endif
data IsJustSym0 :: forall a. Maybe a ~> Bool
type instance Apply IsJustSym0 a = IsJust a

sIsJust :: Sing a -> Sing (IsJust a)
sIsJust SNothing = SFalse
sIsJust (SJust _) = STrue

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

#if __GLASGOW_HASKELL__ >= 810
type Pred :: Nat -> Nat
#endif
type family Pred (a :: Nat) :: Nat where
  Pred Zero = Zero
  Pred (Succ n) = n

#if __GLASGOW_HASKELL__ >= 810
type PredSym0 :: Nat ~> Nat
#endif
data PredSym0 :: Nat ~> Nat
type instance Apply PredSym0 a = Pred a

sPred :: forall (t :: Nat). Sing t -> Sing (Pred t)
sPred SZero = SZero
sPred (SSucc n) = n

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons h t) = Cons (f h) (map f t)

#if __GLASGOW_HASKELL__ >= 810
type Map :: (k1 ~> k2) -> List k1 -> List k2
#endif
type family Map (f :: k1 ~> k2) (l :: List k1) :: List k2 where
    Map f Nil = Nil
    Map f (Cons h t) = Cons (Apply f h) (Map f t)

-- defunctionalization symbols
#if __GLASGOW_HASKELL__ >= 810
type MapSym0 :: forall a b. (a ~> b) ~> List a ~> List b
type MapSym1 :: forall a b. (a ~> b) -> List a ~> List b
#endif
data MapSym0 :: forall a b. (a ~> b) ~> List a ~> List b
data MapSym1 :: forall a b. (a ~> b) -> List a ~> List b
type instance Apply  MapSym0 f     = MapSym1 f
type instance Apply (MapSym1 f) xs = Map f xs

sMap :: forall k1 k2 (a :: List k1) (f :: k1 ~> k2).
       (forall b. Proxy f -> Sing b -> Sing (Apply f b)) -> Sing a -> Sing (Map f a)
sMap _ SNil = SNil
sMap f (SCons h t) = SCons (f Proxy h) (sMap f t)

-- Alternative implementation of sMap with Proxy outside of callback.
-- Not generated by the library.
sMap2 :: forall k1 k2 (a :: List k1) (f :: k1 ~> k2). Proxy f ->
       (forall b. Sing b -> Sing (Apply f b)) -> Sing a -> Sing (Map f a)
sMap2 _ _ SNil = SNil
sMap2 p f (SCons h t) = SCons (f h) (sMap2 p f t)

-- test sMap
foo :: Sing (Cons (Succ (Succ Zero)) (Cons (Succ Zero) Nil))
foo = sMap (\(_ :: Proxy (TyCon1 Succ)) -> SSucc) (SCons (SSucc SZero) (SCons SZero SNil))

-- test sMap2
bar :: Sing (Cons (Succ (Succ Zero)) (Cons (Succ Zero) Nil))
bar = sMap2 (Proxy :: Proxy SuccSym0) (SSucc) (SCons (SSucc SZero) (SCons SZero SNil))

baz :: Sing (Cons Zero (Cons Zero Nil))
baz = sMap2 (Proxy :: Proxy PredSym0) (sPred) (SCons (SSucc SZero) (SCons SZero SNil))

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)
zipWith _ Nil         (Cons _ _)  = Nil
zipWith _ (Cons _ _)  Nil         = Nil
zipWith _ Nil         Nil         = Nil

#if __GLASGOW_HASKELL__ >= 810
type ZipWith :: (a ~> b ~> c) -> List a -> List b -> List c
#endif
type family ZipWith (k1 :: a ~> b ~> c) (k2 :: List a) (k3 :: List b) :: List c where
  ZipWith f (Cons x xs) (Cons y ys) = Cons (Apply (Apply f x) y) (ZipWith f xs ys)
  ZipWith f Nil (Cons z1 z2) = Nil
  ZipWith f (Cons z1 z2) Nil = Nil
  ZipWith f Nil          Nil = Nil

#if __GLASGOW_HASKELL__ >= 810
type ZipWithSym0 :: forall a b c. (a ~> b ~> c) ~> List a ~> List b ~> List c
type ZipWithSym1 :: forall a b c. (a ~> b ~> c) -> List a ~> List b ~> List c
type ZipWithSym2 :: forall a b c. (a ~> b ~> c) -> List a -> List b ~> List c
#endif
data ZipWithSym0 :: forall a b c. (a ~> b ~> c) ~> List a ~> List b ~> List c
data ZipWithSym1 :: forall a b c. (a ~> b ~> c) -> List a ~> List b ~> List c
data ZipWithSym2 :: forall a b c. (a ~> b ~> c) -> List a -> List b ~> List c
type instance Apply  ZipWithSym0 f        = ZipWithSym1 f
type instance Apply (ZipWithSym1 f)    xs = ZipWithSym2 f xs
type instance Apply (ZipWithSym2 f xs) ys = ZipWith f xs ys


sZipWith :: forall a b c (k1 :: a ~> b ~> c) (k2 :: List a) (k3 :: List b).
  (forall (t1 :: a). Proxy k1 -> Sing t1 -> forall (t2 :: b). Sing t2 -> Sing (Apply (Apply k1 t1) t2))
  -> Sing k2 -> Sing k3 -> Sing (ZipWith k1 k2 k3)
sZipWith f (SCons x xs) (SCons y ys) = SCons (f Proxy x y) (sZipWith f xs ys)
sZipWith _ SNil (SCons _ _) = SNil
sZipWith _ (SCons _ _) SNil = SNil
sZipWith _ SNil        SNil = SNil

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l _ (Left x) = l x
either _ r (Right x) = r x

#if __GLASGOW_HASKELL__ >= 810
type Either_ :: (a ~> c) -> (b ~> c) -> Either a b -> c
#endif
type family Either_ (l :: a ~> c) (r :: b ~> c) (e :: Either a b) :: c where
    Either_ l r (Left x) = Apply l x
    Either_ l r (Right x) = Apply r x

-- defunctionalization symbols
#if __GLASGOW_HASKELL__ >= 810
type Either_Sym0 :: forall a c b. (a ~> c) ~> (b ~> c) ~> Either a b ~> c
type Either_Sym1 :: forall a c b. (a ~> c) -> (b ~> c) ~> Either a b ~> c
type Either_Sym2 :: forall a c b. (a ~> c) -> (b ~> c) -> Either a b ~> c
#endif
data Either_Sym0 :: forall a c b. (a ~> c) ~> (b ~> c) ~> Either a b ~> c
data Either_Sym1 :: forall a c b. (a ~> c) -> (b ~> c) ~> Either a b ~> c
data Either_Sym2 :: forall a c b. (a ~> c) -> (b ~> c) -> Either a b ~> c
type instance Apply  Either_Sym0        k1 = Either_Sym1 k1
type instance Apply (Either_Sym1 k1)    k2 = Either_Sym2 k1 k2
type instance Apply (Either_Sym2 k1 k2) k3 = Either_     k1 k2 k3

sEither :: forall a b c
                  (l :: a ~> c)
                  (r :: b ~> c)
                  (e :: Either a b).
           (forall n. Proxy l -> Sing n -> Sing (Apply l n)) ->
           (forall n. Proxy r -> Sing n -> Sing (Apply r n)) ->
           Sing e -> Sing (Either_ l r e)
sEither l _ (SLeft x) = l Proxy x
sEither _ r (SRight x) = r Proxy x

-- Alternative implementation of sEither with Proxy outside of callbacks.
-- Not generated by the library.
sEither2 :: forall a b c
                   (l :: a ~> c)
                   (r :: b ~> c)
                   (e :: Either a b).
           Proxy l -> Proxy r ->
           (forall n. Sing n -> Sing (Apply l n)) ->
           (forall n. Sing n -> Sing (Apply r n)) ->
           Sing e -> Sing (Either_ l r e)
sEither2 _ _ l _ (SLeft  x) = l x
sEither2 _ _ _ r (SRight x) = r x

eitherFoo :: Sing (Succ (Succ Zero))
eitherFoo = sEither (\(_ :: Proxy SuccSym0) -> SSucc)
                    (\(_ :: Proxy PredSym0)     -> sPred) (SLeft (SSucc SZero))

eitherBar :: Sing Zero
eitherBar = sEither2 (Proxy :: Proxy SuccSym0)
                     (Proxy :: Proxy PredSym0)
                     SSucc
                     sPred (SRight (SSucc SZero))

eitherToNat :: Either Nat Nat -> Nat
eitherToNat (Left  x) = x
eitherToNat (Right x) = x

#if __GLASGOW_HASKELL__ >= 810
type EitherToNat :: Either Nat Nat -> Nat
#endif
type family EitherToNat (e :: Either Nat Nat) :: Nat where
    EitherToNat (Left x) = x
    EitherToNat (Right x) = x

sEitherToNat :: Sing a -> Sing (EitherToNat a)
sEitherToNat (SLeft x) = x
sEitherToNat (SRight x) = x

liftMaybe :: (a -> b) -> Maybe a -> Maybe b
liftMaybe _ Nothing = Nothing
liftMaybe f (Just a) = Just (f a)

#if __GLASGOW_HASKELL__ >= 810
type LiftMaybe :: (a ~> b) -> Maybe a -> Maybe b
#endif
type family LiftMaybe (f :: a ~> b) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just (Apply f a)

#if __GLASGOW_HASKELL__ >= 810
type LiftMaybeSym0 :: forall a b. (a ~> b) ~> Maybe a ~> Maybe b
type LiftMaybeSym1 :: forall a b. (a ~> b) -> Maybe a ~> Maybe b
#endif
data LiftMaybeSym0 :: forall a b. (a ~> b) ~> Maybe a ~> Maybe b
data LiftMaybeSym1 :: forall a b. (a ~> b) -> Maybe a ~> Maybe b
type instance Apply  LiftMaybeSym0     k1 = LiftMaybeSym1 k1
type instance Apply (LiftMaybeSym1 k1) k2 = LiftMaybe k1 k2

sLiftMaybe :: forall a b (f :: a ~> b) (x :: Maybe a).
                (forall (y :: a). Proxy f -> Sing y -> Sing (Apply f y)) ->
                Sing x -> Sing (LiftMaybe f x)
sLiftMaybe _ SNothing = SNothing
sLiftMaybe f (SJust a) = SJust (f Proxy a)

(+) :: Nat -> Nat -> Nat
Zero + x = x
(Succ x) + y = Succ (x + y)

#if __GLASGOW_HASKELL__ >= 810
type (+) :: Nat -> Nat -> Nat
#endif
type family (+) (m :: Nat) (n :: Nat) :: Nat where
  Zero + x = x
  (Succ x) + y = Succ (x + y)

-- defunctionalization symbols
#if __GLASGOW_HASKELL__ >= 810
type (+@#@$)  :: Nat ~> Nat ~> Nat
type (+@#@$$) :: Nat -> Nat ~> Nat
#endif
data (+@#@$)  :: Nat ~> Nat ~> Nat
data (+@#@$$) :: Nat -> Nat ~> Nat
type instance Apply  (+@#@$)  k1     = (+@#@$$) k1
type instance Apply ((+@#@$$) k1) k2 = (+) k1 k2

(%+) :: Sing m -> Sing n -> Sing (m + n)
SZero %+ x = x
(SSucc x) %+ y = SSucc (x %+ y)

(-) :: Nat -> Nat -> Nat
Zero - _ = Zero
(Succ x) - Zero = Succ x
(Succ x) - (Succ y) = x - y

#if __GLASGOW_HASKELL__ >= 810
type (-) :: Nat -> Nat -> Nat
#endif
type family (-) (m :: Nat) (n :: Nat) :: Nat where
  Zero - x = Zero
  (Succ x) - Zero = Succ x
  (Succ x) - (Succ y) = x - y

#if __GLASGOW_HASKELL__ >= 810
type (-@#@$)  :: Nat ~> Nat ~> Nat
type (-@#@$$) :: Nat -> Nat ~> Nat
#endif
data (-@#@$)  :: Nat ~> Nat ~> Nat
data (-@#@$$) :: Nat -> Nat ~> Nat
type instance Apply  (-@#@$)  k1     = (-@#@$$) k1
type instance Apply ((-@#@$$) k1) k2 = (-) k1 k2

(%-) :: Sing m -> Sing n -> Sing (m - n)
SZero %- _ = SZero
(SSucc x) %- SZero = SSucc x
(SSucc x) %- (SSucc y) = x %- y

isZero :: Nat -> Bool
isZero n = if n == Zero then True else False

#if __GLASGOW_HASKELL__ >= 810
type IsZero :: Nat -> Bool
#endif
type family IsZero (n :: Nat) :: Bool where
  IsZero n = If (n == Zero) True False

#if __GLASGOW_HASKELL__ >= 810
type IsZeroSym0 :: Nat ~> Bool
#endif
data IsZeroSym0 :: Nat ~> Bool
type instance Apply IsZeroSym0 a = IsZero a

sIsZero :: Sing n -> Sing (IsZero n)
sIsZero n = sIf (n %== SZero) STrue SFalse

{-
(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True
-}

#if __GLASGOW_HASKELL__ >= 810
type (||) :: Bool -> Bool -> Bool
#endif
type family (a :: Bool) || (b :: Bool) :: Bool where
  False || x = x
  True || x = True

#if __GLASGOW_HASKELL__ >= 810
type (||@#@$)  :: Bool ~> Bool ~> Bool
type (||@#@$$) :: Bool -> Bool ~> Bool
#endif
data (||@#@$)  :: Bool ~> Bool ~> Bool
data (||@#@$$) :: Bool -> Bool ~> Bool
type instance Apply (||@#@$) a = (||@#@$$) a
type instance Apply ((||@#@$$) a) b = (||) a b

(%||) :: Sing a -> Sing b -> Sing (a || b)
SFalse %|| x = x
STrue %|| _ = STrue

contains :: Eq a => a -> List a -> Bool
contains _ Nil = False
contains elt (Cons h t) = (elt == h) || contains elt t

#if __GLASGOW_HASKELL__ >= 810
type Contains :: k -> List k -> Bool
#endif
type family Contains (a :: k) (b :: List k) :: Bool where
  Contains elt Nil = False
  Contains elt (Cons h t) = (elt == h) || (Contains elt t)

#if __GLASGOW_HASKELL__ >= 810
type ContainsSym0 :: forall a. a ~> List a ~> Bool
type ContainsSym1 :: forall a. a -> List a ~> Bool
#endif
data ContainsSym0 :: forall a. a ~> List a ~> Bool
data ContainsSym1 :: forall a. a -> List a ~> Bool
type instance Apply  ContainsSym0 a    = ContainsSym1 a
type instance Apply (ContainsSym1 a) b = Contains a b

{-
sContains :: forall k. SEq k =>
             forall (a :: k). Sing a ->
             forall (list :: List k). Sing list -> Sing (Contains a list)
sContains _ SNil = SFalse
sContains elt (SCons h t) = (elt %== h) %|| (sContains elt t)
-}

sContains :: forall a (t1 :: a) (t2 :: List a). SEq a => Sing t1
          -> Sing t2 -> Sing (Contains t1 t2)
sContains _ SNil =
  let lambda :: forall wild. Sing (Contains wild Nil)
      lambda = SFalse
  in
  lambda
sContains elt (SCons h t) =
  let lambda :: forall elt h t. (elt ~ t1, (Cons h t) ~ t2) => Sing elt -> Sing h -> Sing t -> Sing (Contains elt (Cons h t))
      lambda elt' h' t' = (elt' %== h') %|| sContains elt' t'
  in
  lambda elt h t

cont :: Eq a => a -> List a -> Bool
cont = \elt list -> case list of
  Nil -> False
  Cons h t -> (elt == h) || cont elt t

#if __GLASGOW_HASKELL__ >= 810
type Cont :: a ~> List a ~> Bool
#endif
type family Cont :: a ~> List a ~> Bool where
  Cont = Lambda10Sym0

data Lambda10Sym0 f where
  KindInferenceLambda10Sym0 :: (Lambda10Sym0 @@ arg) ~ Lambda10Sym1 arg
                            => Proxy arg
                            -> Lambda10Sym0 f
type instance Lambda10Sym0 `Apply` x = Lambda10Sym1 x

data Lambda10Sym1 a f where
  KindInferenceLambda10Sym1 :: (Lambda10Sym1 a @@ arg) ~ Lambda10Sym2 a arg
                            => Proxy arg
                            -> Lambda10Sym1 a f
type instance (Lambda10Sym1 a) `Apply` b = Lambda10Sym2 a b

type Lambda10Sym2 a b = Lambda10 a b

type family Lambda10 a b where
  Lambda10 elt list = Case10 elt list list

type family Case10 a b scrut where
  Case10 elt list Nil = False
  Case10 elt list (Cons h t) = (||@#@$) @@ ((==@#@$) @@ elt @@ h) @@ (Cont @@ elt @@ t)

data (==@#@$) f where
  (:###==@#@$) :: ((==@#@$) @@ arg) ~ (==@#@$$) arg
               => Proxy arg
               -> (==@#@$) f
type instance (==@#@$) `Apply` x = (==@#@$$) x

data (==@#@$$) a f where
  (:###==@#@$$) :: ((==@#@$$) x @@ arg) ~ (==@#@$$$) x arg
                => Proxy arg
                -> (==@#@$$) x y
type instance (==@#@$$) a `Apply` b = (==) a b

type family (==@#@$$$) a b where
  (==@#@$$$) a b = (==) a b


impNat :: forall m n. SingI n => Proxy n -> Sing m -> Sing (n + m)
impNat _ sm = (sing :: Sing n) %+ sm

callImpNat :: forall n m. Sing n -> Sing m -> Sing (n + m)
callImpNat sn sm = withSingI sn (impNat (Proxy :: Proxy n) sm)

instance Show (SNat n) where
  show SZero = "SZero"
  show (SSucc n) = "SSucc (" ++ (show n) ++ ")"

findIndices :: (a -> Bool) -> [a] -> [Nat]
findIndices p ls = loop Zero ls
  where
    loop _ [] = []
    loop n (x:xs) | p x = n : loop (Succ n) xs
                  | otherwise = loop (Succ n) xs

#if __GLASGOW_HASKELL__ >= 810
type FindIndices :: (a ~> Bool) -> List a -> List Nat
#endif
type family FindIndices (f :: a ~> Bool) (ls :: List a) :: List Nat where
  FindIndices p ls = (Let123LoopSym2 p ls) @@ Zero @@ ls

type family Let123Loop p ls (arg1 :: Nat) (arg2 :: List a) :: List Nat where
  Let123Loop p ls z Nil = Nil
  Let123Loop p ls n (x `Cons` xs) = Case123 p ls n x xs (p @@ x)

type family Case123 p ls n x xs scrut where
  Case123 p ls n x xs True = n `Cons` ((Let123LoopSym2 p ls) @@ (Succ n) @@ xs)
  Case123 p ls n x xs False = (Let123LoopSym2 p ls) @@ (Succ n) @@ xs

data Let123LoopSym2 a b c where
  Let123LoopSym2KindInfernece :: ((Let123LoopSym2 a b @@ z) ~ Let123LoopSym3 a b z)
                              => Proxy z
                              -> Let123LoopSym2 a b c
type instance Apply (Let123LoopSym2 a b) c = Let123LoopSym3 a b c

data Let123LoopSym3 a b c d where
  KindInferenceLet123LoopSym3 :: ((Let123LoopSym3 a b c @@ z) ~ Let123LoopSym4 a b c z)
                              => Proxy z
                              -> Let123LoopSym3 a b c d
type instance Apply (Let123LoopSym3 a b c) d = Let123Loop a b c d

type family Let123LoopSym4 a b c d where
  Let123LoopSym4 a b c d = Let123Loop a b c d

data FindIndicesSym0 a where
  KindInferenceFindIndicesSym0 :: (FindIndicesSym0 @@ z) ~ FindIndicesSym1 z
                               => Proxy z
                               -> FindIndicesSym0 a
type instance Apply FindIndicesSym0 a = FindIndicesSym1 a

data FindIndicesSym1 a b where
  KindInferenceFindIndicesSym1 :: (FindIndicesSym1 a @@ z) ~ FindIndicesSym2 a z
                               => Proxy z
                               -> FindIndicesSym1 a b
type instance Apply (FindIndicesSym1 a) b = FindIndices a b

type family FindIndicesSym2 a b where
  FindIndicesSym2 a b = FindIndices a b

sFindIndices :: forall a (t1 :: a ~> Bool) (t2 :: (List a)).
                Sing t1
             -> Sing t2
             -> Sing (FindIndicesSym0 @@ t1 @@ t2)
sFindIndices sP sLs =
  let sLoop :: forall (u1 :: Nat) (u2 :: List a).
               Sing u1 -> Sing u2
            -> Sing ((Let123LoopSym2 t1 t2) @@ u1 @@ u2)
      sLoop _ SNil = SNil
      sLoop sN (sX `SCons` sXs) = case sP @@ sX of
        STrue -> (singFun2 @ConsSym0 SCons) @@ sN @@
                   ((singFun2 @(Let123LoopSym2 t1 t2) sLoop) @@ ((singFun1 @SuccSym0 SSucc) @@ sN) @@ sXs)
        SFalse -> (singFun2 @(Let123LoopSym2 t1 t2) sLoop) @@ ((singFun1 @SuccSym0 SSucc) @@ sN) @@ sXs
  in
  (singFun2 @(Let123LoopSym2 t1 t2) sLoop) @@ SZero @@ sLs


fI :: forall a. (a -> Bool) -> [a] -> [Nat]
fI = \p ls ->
  let loop :: Nat -> [a] -> [Nat]
      loop _ [] = []
      loop n (x:xs) = case p x of
                        True -> n : loop (Succ n) xs
                        False -> loop (Succ n) xs
  in
  loop Zero ls

type FI = Lambda22Sym0

type FISym0 = FI

type family Lambda22 p ls where
  Lambda22 p ls = (Let123LoopSym2 p ls) @@ Zero @@ ls

data Lambda22Sym0 a where
  KindInferenceLambda22Sym0 :: (Lambda22Sym0 @@ z) ~ Lambda22Sym1 z
                            => Proxy z
                            -> Lambda22Sym0 a
type instance Apply Lambda22Sym0 a = Lambda22Sym1 a

data Lambda22Sym1 a b where
  KindInferenceLambda22Sym1 :: (Lambda22Sym1 a @@ z) ~ Lambda22Sym2 a z
                            => Proxy z
                            -> Lambda22Sym1 a b
type instance Apply (Lambda22Sym1 a) b = Lambda22 a b

type family Lambda22Sym2 a b where
  Lambda22Sym2 a b = Lambda22 a b

{-
sFI :: forall a (t1 :: a ~> Bool) (t2 :: List a). Sing t1
    -> Sing t2
    -> Sing (FISym0 @@ t1 @@ t2)
sFI = unSingFun2 (singFun2 @FI (\p ls ->
    let lambda :: forall {-(t1 :: a ~> Bool)-} t1 t2. Sing t1 -> Sing t2 -> Sing (Lambda22Sym0 @@ t1 @@ t2)
        lambda sP sLs =
          let sLoop :: (Lambda22Sym0 @@ t1 @@ t2) ~ (Let123LoopSym2 t1 t2 @@ Zero @@ t2) => forall (u1 :: Nat). Sing u1
                    -> forall {-(u2 :: List a)-} u2. Sing u2
                    -> Sing ((Let123LoopSym2 t1 t2) @@ u1 @@ u2)
              sLoop _ SNil = SNil
              sLoop sN (sX `SCons` sXs) =  case sP @@ sX of
                STrue -> (singFun2 @ConsSym0 SCons) @@ sN @@
                     ((singFun2 @(Let123LoopSym2 t1 t2) sLoop) @@ ((singFun1 @SuccSym0 SSucc) @@ sN) @@ sXs)
                SFalse -> (singFun2 @(Let123LoopSym2 t1 t2) sLoop) @@ ((singFun1 @SuccSym0 SSucc) @@ sN) @@ sXs
          in
          (singFun2 @(Let123LoopSym2 t1 t2) sLoop) @@ SZero @@ sLs
    in
    lambda p ls
  ))
-}

------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 810
type G :: Type -> Type
#endif
data G :: Type -> Type where
  MkG :: G Bool

#if __GLASGOW_HASKELL__ >= 810
type SG :: forall a. G a -> Type
#endif
data SG :: forall a. G a -> Type where
  SMkG :: SG MkG
#if __GLASGOW_HASKELL__ >= 808
type instance Sing @(G a) =
#else
type instance Sing =
#endif
  SG
