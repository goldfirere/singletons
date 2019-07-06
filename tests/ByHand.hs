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
             LambdaCase, TemplateHaskell, EmptyCase,
             TypeApplications, EmptyCase
 #-}

module ByHand where

import Data.Kind (Type)
import Prelude hiding (Bool, False, True, Maybe, Just, Nothing, Either, Left, Right, map, zipWith,
                       (&&), (||), (+), (-))
import Unsafe.Coerce

import Data.Type.Equality hiding (type (==), apply)
import Data.Proxy

import Data.Singletons
import Data.Singletons.Decide

-----------------------------------
-- Original ADTs ------------------
-----------------------------------

data Nat :: Type where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Eq

data Bool :: Type where
  False :: Bool
  True :: Bool

data Maybe :: Type -> Type where
  Nothing :: Maybe a
  Just :: a -> Maybe a
  deriving Eq

-- Defined using names to avoid fighting with concrete syntax
data List :: Type -> Type where
  Nil :: List a
  Cons :: a -> List a -> List a
  deriving Eq

data Either :: Type -> Type -> Type where
  Left :: a -> Either a b
  Right :: b -> Either a b


-----------------------------------
-- One-time definitions -----------
-----------------------------------

-- Promoted equality type class
class PEq k where
  type (==) (a :: k) (b :: k) :: Bool
  -- omitting definition of /=

-- Singleton type equality type class
class SEq k where
  (%==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a == b)
  -- omitting definition of %/=

type family If cond tru fls where
  If True  tru  fls = tru
  If False tru  fls = fls

sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

-----------------------------------
-- Auto-generated code ------------
-----------------------------------

-- Nat

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
type instance Sing = SNat

data SuccSym0 :: Nat ~> Nat
type instance Apply SuccSym0 x = Succ x

type family EqualsNat (a :: Nat) (b :: Nat) where
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
instance SingKind Nat where
  type Demote Nat = Nat
  fromSing SZero = Zero
  fromSing (SSucc n) = Succ (fromSing n)
  toSing Zero = SomeSing SZero
  toSing (Succ n) = withSomeSing n (\n' -> SomeSing $ SSucc n')

-- Bool

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue :: SBool True
type instance Sing = SBool

(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && x = x

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
instance SingKind Bool where
  type Demote Bool = Bool
  fromSing SFalse = False
  fromSing STrue = True
  toSing False = SomeSing SFalse
  toSing True  = SomeSing STrue

-- Maybe

data SMaybe :: forall k. Maybe k -> Type where
  SNothing :: SMaybe Nothing
  SJust :: forall k (a :: k). Sing a -> SMaybe (Just a)
type instance Sing = SMaybe

type family EqualsMaybe (a :: Maybe k) (b :: Maybe k) where
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
instance SingKind k => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe (Demote k)
  fromSing SNothing = Nothing
  fromSing (SJust a) = Just (fromSing a)
  toSing Nothing = SomeSing SNothing
  toSing (Just x) =
    case toSing x :: SomeSing k of
      SomeSing x' -> SomeSing $ SJust x'

-- List

data SList :: forall k. List k -> Type where
  SNil :: SList Nil
  SCons :: forall k (h :: k) (t :: List k). Sing h -> SList t -> SList (Cons h t)
type instance Sing = SList

type NilSym0 = Nil

data ConsSym0 :: a ~> List a ~> List a
type instance Apply ConsSym0 a = ConsSym1 a

data ConsSym1 :: a -> List a ~> List a
type instance Apply (ConsSym1 a) b = ConsSym2 a b

type ConsSym2 a b = Cons a b

type family EqualsList (a :: List k) (b :: List k) where
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
instance SingKind k => SingKind (List k) where
  type Demote (List k) = List (Demote k)
  fromSing SNil = Nil
  fromSing (SCons h t) = Cons (fromSing h) (fromSing t)
  toSing Nil = SomeSing SNil
  toSing (Cons h t) =
    case ( toSing h :: SomeSing k
         , toSing t :: SomeSing (List k) ) of
      (SomeSing h', SomeSing t') -> SomeSing $ SCons h' t'

-- Either

data SEither :: forall k1 k2. Either k1 k2 -> Type where
  SLeft :: forall k1 (a :: k1). Sing a -> SEither (Left a)
  SRight :: forall k2 (b :: k2). Sing b -> SEither (Right b)
type instance Sing = SEither

instance (SingI a) => SingI (Left (a :: k)) where
  sing = SLeft sing
instance (SingI b) => SingI (Right (b :: k)) where
  sing = SRight sing
instance (SingKind k1, SingKind k2) => SingKind (Either k1 k2) where
  type Demote (Either k1 k2) = Either (Demote k1) (Demote k2)
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

data Composite :: Type -> Type -> Type where
  MkComp :: Either (Maybe a) b -> Composite a b

data SComposite :: forall k1 k2. Composite k1 k2 -> Type where
  SMkComp :: forall k1 k2 (a :: Either (Maybe k1) k2). SEither a -> SComposite (MkComp a)
type instance Sing = SComposite

instance SingI a => SingI (MkComp (a :: Either (Maybe k1) k2)) where
  sing = SMkComp sing
instance (SingKind k1, SingKind k2) => SingKind (Composite k1 k2) where
  type Demote (Composite k1 k2) =
    Composite (Demote k1) (Demote k2)
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

data Empty
data SEmpty :: Empty -> Type
type instance Sing = SEmpty
instance SingKind Empty where
  type Demote Empty = Empty
  fromSing = \case
  toSing x = SomeSing (case x of)

-- Type

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

data Rep = Nat | Maybe Rep | Vec Rep Nat

data SRep :: Type -> Type where
  SNat :: SRep Nat
  SMaybe :: SRep a -> SRep (Maybe a)
  SVec :: SRep a -> SNat n -> SRep (Vec a n)
type instance Sing = SRep

instance SingI Nat where
  sing = SNat
instance SingI a => SingI (Maybe a) where
  sing = SMaybe sing
instance (SingI a, SingI n) => SingI (Vec a n) where
  sing = SVec sing sing

instance SingKind Type where
  type Demote Type = Rep

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

type family EqualsType (a :: Type) (b :: Type) where
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

type family IsJust (a :: Maybe k) :: Bool where
    IsJust Nothing = False
    IsJust (Just a) = True

-- defunctionalization symbols
data IsJustSym0 :: Maybe a ~> Bool
type instance Apply IsJustSym0 a = IsJust a

sIsJust :: Sing a -> Sing (IsJust a)
sIsJust SNothing = SFalse
sIsJust (SJust _) = STrue

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

type family Pred (a :: Nat) :: Nat where
  Pred Zero = Zero
  Pred (Succ n) = n

data PredSym0 :: Nat ~> Nat
type instance Apply PredSym0 a = Pred a

sPred :: forall (t :: Nat). Sing t -> Sing (Pred t)
sPred SZero = SZero
sPred (SSucc n) = n

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons h t) = Cons (f h) (map f t)

type family Map (f :: k1 ~> k2) (l :: List k1) :: List k2 where
    Map f Nil = Nil
    Map f (Cons h t) = Cons (Apply f h) (Map f t)

-- defunctionalization symbols
data MapSym1 :: (a ~> b) -> List a ~> List b
data MapSym0 :: (a ~> b) ~> List a ~> List b
type instance Apply (MapSym1 f) xs = Map f xs
type instance Apply  MapSym0 f     = MapSym1 f

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

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ [] (_:_)      = []
zipWith _ (_:_) []      = []
zipWith _ []    []      = []

type family ZipWith (k1 :: a ~> b ~> c)
                    (k2 :: List a)
                    (k3 :: List b) :: List c where
  ZipWith f (Cons x xs) (Cons y ys) = Cons (Apply (Apply f x) y) (ZipWith f xs ys)
  ZipWith f Nil (Cons z1 z2) = Nil
  ZipWith f (Cons z1 z2) Nil = Nil
  ZipWith f Nil          Nil = Nil

data ZipWithSym2 :: (a ~> b ~> c) -> List a -> List b ~> List c
data ZipWithSym1 :: (a ~> b ~> c) -> List a ~> List b ~> List c
data ZipWithSym0 :: (a ~> b ~> c) ~> List a ~> List b ~> List c
type instance Apply (ZipWithSym2 f xs) ys = ZipWith f xs ys
type instance Apply (ZipWithSym1 f)    xs = ZipWithSym2 f xs
type instance Apply  ZipWithSym0 f        = ZipWithSym1 f


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

type family Either_ (l :: a ~> c) (r :: b ~> c) (e :: Either a b) :: c where
    Either_ l r (Left x) = Apply l x
    Either_ l r (Right x) = Apply r x

-- defunctionalization symbols
data Either_Sym2 :: (a ~> c) -> (b ~> c) -> Either a b ~> c
data Either_Sym1 :: (a ~> c) -> (b ~> c) ~> Either a b ~> c
data Either_Sym0 :: (a ~> c) ~> (b ~> c) ~> Either a b ~> c
type instance Apply (Either_Sym2 k1 k2) k3 = Either_     k1 k2 k3
type instance Apply (Either_Sym1 k1)    k2 = Either_Sym2 k1 k2
type instance Apply  Either_Sym0        k1 = Either_Sym1 k1

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

type family EitherToNat (e :: Either Nat Nat) :: Nat where
    EitherToNat (Left x) = x
    EitherToNat (Right x) = x

sEitherToNat :: Sing a -> Sing (EitherToNat a)
sEitherToNat (SLeft x) = x
sEitherToNat (SRight x) = x

liftMaybe :: (a -> b) -> Maybe a -> Maybe b
liftMaybe _ Nothing = Nothing
liftMaybe f (Just a) = Just (f a)

type family LiftMaybe (f :: a ~> b) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just (Apply f a)

data LiftMaybeSym1 :: (a ~> b) -> Maybe a ~> Maybe b
data LiftMaybeSym0 :: (a ~> b) ~> Maybe a ~> Maybe b
type instance Apply (LiftMaybeSym1 k1) k2 = LiftMaybe k1 k2
type instance Apply  LiftMaybeSym0     k1 = LiftMaybeSym1 k1

sLiftMaybe :: forall a b (f :: a ~> b) (x :: Maybe a).
                (forall (y :: a). Proxy f -> Sing y -> Sing (Apply f y)) ->
                Sing x -> Sing (LiftMaybe f x)
sLiftMaybe _ SNothing = SNothing
sLiftMaybe f (SJust a) = SJust (f Proxy a)

(+) :: Nat -> Nat -> Nat
Zero + x = x
(Succ x) + y = Succ (x + y)

type family (+) (m :: Nat) (n :: Nat) :: Nat where
  Zero + x = x
  (Succ x) + y = Succ (x + y)

-- defunctionalization symbols
data (+$$) :: Nat -> Nat ~> Nat
data (+$)  :: Nat ~> Nat ~> Nat
type instance Apply ((+$$) k1) k2 = (+) k1 k2
type instance Apply  (+$)  k1     = (+$$) k1

(%+) :: Sing m -> Sing n -> Sing (m + n)
SZero %+ x = x
(SSucc x) %+ y = SSucc (x %+ y)

(-) :: Nat -> Nat -> Nat
Zero - _ = Zero
(Succ x) - Zero = Succ x
(Succ x) - (Succ y) = x - y

type family (-) (m :: Nat) (n :: Nat) :: Nat where
  Zero - x = Zero
  (Succ x) - Zero = Succ x
  (Succ x) - (Succ y) = x - y

data (-$$) :: Nat -> Nat ~> Nat
data (-$)  :: Nat ~> Nat ~> Nat
type instance Apply ((-$$) k1) k2 = (-) k1 k2
type instance Apply  (-$)  k1     = (-$$) k1

(%-) :: Sing m -> Sing n -> Sing (m - n)
SZero %- _ = SZero
(SSucc x) %- SZero = SSucc x
(SSucc x) %- (SSucc y) = x %- y

isZero :: Nat -> Bool
isZero n = if n == Zero then True else False

type family IsZero (n :: Nat) :: Bool where
  IsZero n = If (n == Zero) True False

data IsZeroSym0 :: Nat ~> Bool
type instance Apply IsZeroSym0 a = IsZero a

sIsZero :: Sing n -> Sing (IsZero n)
sIsZero n = sIf (n %== SZero) STrue SFalse

(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True

type family (a :: Bool) || (b :: Bool) :: Bool where
  False || x = x
  True || x = True

data (||$$) :: Bool -> Bool ~> Bool
data (||$)  :: Bool ~> Bool ~> Bool
type instance Apply ((||$$) a) b = (||) a b
type instance Apply (||$) a = (||$$) a

(%||) :: Sing a -> Sing b -> Sing (a || b)
SFalse %|| x = x
STrue %|| _ = STrue

{-
contains :: Eq a => a -> List a -> Bool
contains _ Nil = False
contains elt (Cons h t) = (elt == h) || contains elt t
-}

type family Contains (a :: k) (b :: List k) :: Bool where
  Contains elt Nil = False
  Contains elt (Cons h t) = (elt == h) || (Contains elt t)

data ContainsSym1 :: a -> List a ~> Bool
data ContainsSym0 :: a ~> List a ~> Bool
type instance Apply (ContainsSym1 a) b = Contains a b
type instance Apply  ContainsSym0 a    = ContainsSym1 a

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

{-
cont :: Eq a => a -> List a -> Bool
cont = \elt list -> case list of
  Nil -> False
  Cons h t -> (elt == h) || cont elt t
-}

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
  Case10 elt list (Cons h t) = (||$) @@ ((==$) @@ elt @@ h) @@ (Cont @@ elt @@ t)

data (==$) f where
  (:###==$) :: ((==$) @@ arg) ~ (==$$) arg
            => Proxy arg
            -> (==$) f
type instance (==$) `Apply` x = (==$$) x

data (==$$) a f where
  (:###==$$) :: ((==$$) x @@ arg) ~ (==$$$) x arg
             => Proxy arg
             -> (==$$) x y
type instance (==$$) a `Apply` b = (==$$$) a b

type (==$$$) a b = (==) a b


impNat :: forall m n. SingI n => Proxy n -> Sing m -> Sing (n + m)
impNat _ sm = (sing :: Sing n) %+ sm

callImpNat :: forall n m. Sing n -> Sing m -> Sing (n + m)
callImpNat sn sm = withSingI sn (impNat (Proxy :: Proxy n) sm)

instance Show (SNat n) where
  show SZero = "SZero"
  show (SSucc n) = "SSucc (" ++ (show n) ++ ")"

{-
findIndices :: (a -> Bool) -> [a] -> [Nat]
findIndices p ls = loop Zero ls
  where
    loop _ [] = []
    loop n (x:xs) | p x = n : loop (Succ n) xs
                  | otherwise = loop (Succ n) xs
-}

findIndices' :: forall a. (a -> Bool) -> [a] -> [Nat]
findIndices' p ls =
  let loop :: Nat -> [a] -> [Nat]
      loop _ [] = []
      loop n (x:xs) = case p x of
                        True -> n : loop (Succ n) xs
                        False -> loop (Succ n) xs
  in
  loop Zero ls

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
type instance Apply (Let123LoopSym3 a b c) d = Let123LoopSym4 a b c d

type Let123LoopSym4 a b c d = Let123Loop a b c d

data FindIndicesSym0 a where
  KindInferenceFindIndicesSym0 :: (FindIndicesSym0 @@ z) ~ FindIndicesSym1 z
                               => Proxy z
                               -> FindIndicesSym0 a
type instance Apply FindIndicesSym0 a = FindIndicesSym1 a

data FindIndicesSym1 a b where
  KindInferenceFindIndicesSym1 :: (FindIndicesSym1 a @@ z) ~ FindIndicesSym2 a z
                               => Proxy z
                               -> FindIndicesSym1 a b
type instance Apply (FindIndicesSym1 a) b = FindIndicesSym2 a b

type FindIndicesSym2 a b = FindIndices a b

sFindIndices :: forall a (t1 :: a ~> Bool) (t2 :: (List a)).
                Sing t1
             -> Sing t2
             -> Sing (FindIndicesSym0 @@ t1 @@ t2)
sFindIndices sP sLs =
  let sLoop :: forall (u1 :: Nat). Sing u1
            -> forall (u2 :: List a). Sing u2
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
type instance Apply (Lambda22Sym1 a) b = Lambda22Sym2 a b

type Lambda22Sym2 a b = Lambda22 a b

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

data G :: Type -> Type where
  MkG :: G Bool

data SG :: forall a. G a -> Type where
  SMkG :: SG MkG
type instance Sing = SG
