{- ByHand.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

Shows the derivations for the singleton definitions done by hand.
This file is a great way to understand the singleton encoding better.

-}

{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, KindSignatures, GADTs,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             RankNTypes, TypeOperators, MultiParamTypeClasses,
             FunctionalDependencies, ScopedTypeVariables, CPP
 #-}

module ByHand where

import Prelude hiding (Maybe, Just, Nothing, Either, Left, Right, map,
                       Bool, False, True, (+), (-))

#if __GLASGOW_HASKELL__ >= 707

import GHC.TypeLits ( KindIs(..), Sing, SingI(..), SingE(..), SingRep )

#endif

-----------------------------------
-- Original ADTs ------------------
-----------------------------------

data Nat :: * where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Eq

data Bool :: * where
  False :: Bool
  True :: Bool

-- This is necessary for defining boolean equality at the type level
type family (a :: Bool) :&& (b :: Bool) :: Bool
type instance False :&& False = False
type instance False :&& True = False
type instance True :&& False = False
type instance True :&& True = True

data Maybe :: * -> * where
  Nothing :: Maybe a
  Just :: a -> Maybe a
  deriving Eq

-- Defined using names to avoid fighting with concrete syntax
data List :: * -> * where
  Nil :: List a
  Cons :: a -> List a -> List a
  deriving Eq

data Either :: * -> * -> * where
  Left :: a -> Either a b
  Right :: b -> Either a b

-----------------------------------
-- One-time definitions -----------
-----------------------------------

-- Type-level boolean equality
type family (a :: k) :==: (b :: k) :: Bool

-- Singleton type equality type class
class (t ~ KindParam) => SEq (t :: KindIs k) where
  (%==%) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :==: b)

-- Singleton existential (still experimental)
data Existential (kparam :: KindIs k) =
  forall (a :: k). SingRep a => Exists (Sing a)

-- A way to store instances of SingRep at runtime
data SingInstance :: k -> * where 
  SingInstance :: SingRep a => SingInstance a

-- A "kind class" marking those kinds that are considered in our
-- universe. Any type of a kind that has a SingKind instance can
-- be made into a singleton.
class (kparam ~ KindParam) => SingKind (kparam :: KindIs k) where
  singInstance :: forall (a :: k). Sing a -> SingInstance a

-- A class for the generation of singleton existentials (experimental)
class ( SingKind (kparam :: KindIs k)
      , a ~ DemoteRep (KindParam :: KindIs k)
      , kparam ~ KindParam ) =>
      HasSingleton a (kparam :: KindIs k) | a -> kparam where
  exists :: a -> Existential kparam

#if __GLASGOW_HASKELL__ < 707

-- These definitions are all copies of the definitions in an up-to-date GHC.TypeLits

-- Kind-level proxy
data KindIs (k :: *) = KindParam

-- The Sing family
data family Sing (a :: k)

-- Introduction and elimination type classes
class SingI (a :: k) where
  sing :: Sing a
class (kparam ~ KindParam) => SingE (kparam :: KindIs k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam

-- Convenient abbreviations
type KindOf (a :: k) = (KindParam :: KindIs k)
type Demote (a :: k) = DemoteRep (KindParam :: KindIs k)

-- SingRep is a synonym for a combination of SingI and SingE
class    (SingI a, SingE (KindOf a)) => SingRep (a :: k)
instance (SingI a, SingE (KindOf a)) => SingRep (a :: k)

#endif

type family If (a :: Bool) (b :: k) (c :: k) :: k where
  If True b c = b
  If False b c = c

sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

-----------------------------------
-- Auto-generated code ------------
-----------------------------------

-- Nat

data instance Sing (a :: Nat) where
  SZero :: Sing Zero
  SSucc :: SingRep n => Sing n -> Sing (Succ n)

#if __GLASGOW_HASKELL__ >= 707

type family EqualsNat (a :: Nat) (b :: Nat) where
  EqualsNat Zero Zero = True
  EqualsNat (Succ a) (Succ b) = a :==: b
  EqualsNat (n1 :: Nat) (n2 :: Nat) = False
type instance (a :: Nat) :==: (b :: Nat) = EqualsNat a b

#else

type instance Zero :==: Zero = True
type instance Zero :==: (Succ n) = False
type instance (Succ n) :==: Zero = False
type instance (Succ n) :==: (Succ n') = n :==: n'

#endif

instance SEq (KindParam :: KindIs Nat) where
  SZero %==% SZero = STrue
  SZero %==% (SSucc _) = SFalse
  (SSucc _) %==% SZero = SFalse
  (SSucc n) %==% (SSucc n') = n %==% n'

sZero :: Sing Zero
sZero = SZero

sSucc :: Sing n -> Sing (Succ n)
sSucc n = case singInstance n of
  SingInstance -> SSucc n

instance SingI Zero where
  sing = SZero
instance SingRep n => SingI (Succ n) where
  sing = SSucc sing
instance SingE (KindParam :: KindIs Nat) where
  type DemoteRep (KindParam:: KindIs Nat) = Nat
  fromSing SZero = Zero
  fromSing (SSucc n) = Succ (fromSing n)
instance HasSingleton Nat (KindParam :: KindIs Nat) where
  exists Zero = Exists SZero
  exists (Succ n) = case exists n of Exists n' -> Exists (SSucc n')
instance SingKind (KindParam :: KindIs Nat) where
  singInstance SZero = SingInstance
  singInstance (SSucc _) = SingInstance

-- Bool

data instance Sing (a :: Bool) where
  SFalse :: Sing False
  STrue :: Sing True

sFalse :: Sing False
sFalse = SFalse

sTrue :: Sing True
sTrue = STrue

(%:&&) :: forall (a :: Bool) (b :: Bool). Sing a -> Sing b -> Sing (a :&& b)
SFalse %:&& SFalse = SFalse
SFalse %:&& STrue = SFalse
STrue %:&& SFalse = SFalse
STrue %:&& STrue = STrue

instance SingI False where
  sing = SFalse
instance SingI True where
  sing = STrue
instance SingE (KindParam :: KindIs Bool) where
  type DemoteRep (KindParam :: KindIs Bool) = Bool
  fromSing SFalse = False
  fromSing STrue = True
instance HasSingleton Bool (KindParam :: KindIs Bool) where
  exists False = Exists SFalse
  exists True = Exists STrue
instance SingKind (KindParam :: KindIs Bool) where
  singInstance SFalse = SingInstance
  singInstance STrue = SingInstance


-- Maybe

data instance Sing (a :: Maybe k) where
  SNothing :: Sing Nothing
  SJust :: forall (a :: k). (SingKind (KindParam :: KindIs k), SingRep a) =>
             Sing a -> Sing (Just a)

#if __GLASGOW_HASKELL__ >= 707

type family EqualsMaybe (a :: Maybe k) (b :: Maybe k) where
  EqualsMaybe Nothing Nothing = True
  EqualsMaybe (Just a) (Just a') = a :==: a'
  EqualsMaybe (x :: Maybe k) (y :: Maybe k) = False
type instance (a :: Maybe k) :==: (b :: Maybe k) = EqualsMaybe a b

#else

type instance Nothing :==: Nothing = True
type instance Nothing :==: (Just a) = False
type instance (Just a) :==: Nothing = False
type instance (Just a) :==: (Just a') = a :==: a'

#endif

instance SEq (KindParam :: KindIs k) => SEq (KindParam :: KindIs (Maybe k)) where
  SNothing %==% SNothing = STrue
  SNothing %==% (SJust _) = SFalse
  (SJust _) %==% SNothing = SFalse
  (SJust a) %==% (SJust a') = a %==% a'

sNothing :: Sing (Nothing :: Maybe k)
sNothing = SNothing

sJust :: forall (a :: k). SingKind (KindParam :: KindIs k) => Sing a -> Sing (Just a)
sJust a = case singInstance a of
  SingInstance -> SJust a

instance SingI (Nothing :: Maybe k) where
  sing = SNothing
instance (SingKind (KindParam :: KindIs k), SingRep a) => SingI (Just (a :: k)) where
  sing = SJust sing
instance SingE (KindParam :: KindIs (Maybe k)) where
  type DemoteRep (KindParam :: KindIs (Maybe k)) = Maybe (DemoteRep (KindParam :: KindIs k))
  fromSing SNothing = Nothing
  fromSing (SJust a) = Just (fromSing a)
instance HasSingleton a (KindParam :: KindIs k) =>
           HasSingleton (Maybe a) (KindParam :: KindIs (Maybe k)) where
  exists Nothing = Exists SNothing
  exists (Just a) = case exists a of Exists a' -> Exists (SJust a')
instance SingKind (KindParam :: KindIs (Maybe k)) where
  singInstance SNothing = SingInstance
  singInstance (SJust _) = SingInstance

-- List

data instance Sing (a :: List k) where
  SNil :: Sing Nil
  SCons :: forall (h :: k) (t :: List k).
             (SingKind (KindParam :: KindIs k), SingKind (KindParam :: KindIs (List k)),
              SingRep h, SingRep t) =>
             Sing h -> Sing t -> Sing (Cons h t)

#if __GLASGOW_HASKELL__ >= 707

type family EqualsList (a :: List k) (b :: List k) where
  EqualsList Nil Nil = True
  EqualsList (Cons a b) (Cons a' b') = (a :==: a') :&& (b :==: b')
  EqualsList (x :: List k) (y :: List k) = False
type instance (a :: List k) :==: (b :: List k) = EqualsList a b

#else

type instance Nil :==: Nil = True
type instance Nil :==: (Cons a b) = False
type instance (Cons a b) :==: Nil = False
type instance (Cons a b) :==: (Cons a' b') = (a :==: a') :&& (b :==: b')

#endif

instance SEq (KindParam :: KindIs k) => SEq (KindParam :: KindIs (List k)) where
  SNil %==% SNil = STrue
  SNil %==% (SCons _ _) = SFalse
  (SCons _ _) %==% SNil = SFalse
  (SCons a b) %==% (SCons a' b') = (a %==% a') %:&& (b %==% b')

sNil :: Sing Nil
sNil = SNil

sCons :: forall (h :: k) (t :: List k).
           SingKind (KindParam :: KindIs k) =>
           Sing h -> Sing t -> Sing (Cons h t)
sCons h t = case (singInstance h, singInstance t) of
  (SingInstance, SingInstance) -> SCons h t

instance SingI Nil where
  sing = SNil
instance (SingKind (KindParam :: KindIs k), SingRep h, SingRep t) =>
           SingI (Cons (h :: k) (t :: List k)) where
  sing = SCons sing sing
instance SingE (KindParam :: KindIs (List k)) where
  type DemoteRep (KindParam :: KindIs (List k)) = List (DemoteRep (KindParam :: KindIs k))
  fromSing SNil = Nil
  fromSing (SCons h t) = Cons (fromSing h) (fromSing t)
instance HasSingleton a (KindParam :: KindIs k) =>
           HasSingleton (List a) (KindParam :: KindIs (List k)) where
  exists Nil = Exists SNil
  exists (Cons h t) =
    case exists h of
      Exists h' -> case exists t of
        Exists t' -> Exists (SCons h' t')
instance SingKind (KindParam :: KindIs (List k)) where
  singInstance SNil = SingInstance
  singInstance (SCons _ _) = SingInstance

-- Either

data instance Sing (a :: Either k1 k2) where
  SLeft :: forall (a :: k).
             (SingKind (KindParam :: KindIs k), SingRep a) => Sing a -> Sing (Left a)
  SRight :: forall (b :: k).
             (SingKind (KindParam :: KindIs k), SingRep b) => Sing b -> Sing (Right b)

sLeft :: forall (a :: k). SingKind (KindParam :: KindIs k) => Sing a -> Sing (Left a)
sLeft x = case singInstance x of
  SingInstance -> SLeft x

sRight :: forall (a :: k). SingKind (KindParam :: KindIs k) => Sing a -> Sing (Right a)
sRight x = case singInstance x of
  SingInstance -> SRight x 

instance (SingKind (KindParam :: KindIs k), SingRep a) => SingI (Left (a :: k)) where
  sing = SLeft sing
instance (SingKind (KindParam :: KindIs k), SingRep b) => SingI (Right (b :: k)) where
  sing = SRight sing
instance SingE (KindParam :: KindIs (Either k1 k2)) where
  type DemoteRep (KindParam :: KindIs (Either k1 k2)) =
    Either (DemoteRep (KindParam :: KindIs k1)) (DemoteRep (KindParam :: KindIs k2))
  fromSing (SLeft x) = Left (fromSing x)
  fromSing (SRight x) = Right (fromSing x)
instance (HasSingleton a (KindParam :: KindIs ak), HasSingleton b (KindParam :: KindIs bk)) =>
           HasSingleton (Either a b) (KindParam :: KindIs (Either ak bk)) where
  exists (Left x) = case exists x of Exists x' -> Exists (SLeft x')
  exists (Right x) = case exists x of Exists x' -> Exists (SRight x')
instance SingKind (KindParam :: KindIs (Either k1 k2)) where
  singInstance (SLeft _) = SingInstance
  singInstance (SRight _) = SingInstance

-----------------------------------
-- Some example functions ---------
-----------------------------------

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

type family IsJust (a :: Maybe k) :: Bool
type instance IsJust Nothing = False
type instance IsJust (Just a) = True

sIsJust :: Sing a -> Sing (IsJust a)
sIsJust SNothing = sFalse
sIsJust (SJust _) = sTrue

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons h t) = Cons (f h) (map f t)

type family Map (f :: k1 -> k2) (l :: List k1) :: List k2
type instance Map f Nil = Nil
type instance Map f (Cons h t) = Cons (f h) (Map f t)

sMap :: forall (a :: List k1) (f :: k1 -> k2).
          SingKind (KindParam :: KindIs k2) =>
          (forall b. Sing b -> Sing (f b)) -> Sing a -> Sing (Map f a)
sMap _ SNil = sNil
sMap f (SCons h t) = sCons (f h) (sMap f t)

-- test sMap
foo = sMap sSucc (SCons (SSucc SZero) (SCons SZero SNil))

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l _ (Left x) = l x
either _ r (Right x) = r x

type family EEither (l :: a -> c) (r :: b -> c) (e :: Either a b) :: c
type instance EEither l r (Left x) = l x
type instance EEither l r (Right x) = r x

sEither :: (forall a. Sing a -> Sing (l a)) ->
           (forall a. Sing a -> Sing (r a)) ->
           Sing e -> Sing (EEither l r e)
sEither l _ (SLeft x) = l x
sEither _ r (SRight x) = r x

eitherToNat :: Either Nat Nat -> Nat
eitherToNat (Left x) = x
eitherToNat (Right x) = x

type family EitherToNat (e :: Either Nat Nat) :: Nat
type instance EitherToNat (Left x) = x
type instance EitherToNat (Right x) = x

sEitherToNat :: Sing a -> Sing (EitherToNat a)
sEitherToNat (SLeft x) = x
sEitherToNat (SRight x) = x

liftMaybe :: (a -> b) -> Maybe a -> Maybe b
liftMaybe _ Nothing = Nothing
liftMaybe f (Just a) = Just (f a)

type family LiftMaybe (f :: a -> b) (x :: Maybe a) :: Maybe b
type instance LiftMaybe f Nothing = Nothing
type instance LiftMaybe f (Just a) = Just (f a)

sLiftMaybe :: forall (f :: a -> b) (x :: Maybe a).
                SingKind (KindParam :: KindIs b) =>
                (forall (y :: a). Sing y -> Sing (f y)) ->
                Sing x -> Sing (LiftMaybe f x)
sLiftMaybe _ SNothing = SNothing
sLiftMaybe f (SJust a) = sJust (f a)

(+) :: Nat -> Nat -> Nat
Zero + x = x
(Succ x) + y = Succ (x + y)

type family (:+) (m :: Nat) (n :: Nat) :: Nat where
  Zero :+ x = x
  (Succ x) :+ y = Succ (x :+ y)

(%:+) :: Sing m -> Sing n -> Sing (m :+ n)
SZero %:+ x = x
(SSucc x) %:+ y = sSucc (x %:+ y)

(-) :: Nat -> Nat -> Nat
Zero - _ = Zero
(Succ x) - Zero = Succ x
(Succ x) - (Succ y) = x - y

type family (:-) (m :: Nat) (n :: Nat) :: Nat where
  Zero :- x = Zero
  (Succ x) :- Zero = Succ x
  (Succ x) :- (Succ y) = x :- y

(%:-) :: Sing m -> Sing n -> Sing (m :- n)
SZero %:- _ = SZero
(SSucc x) %:- SZero = sSucc x
(SSucc x) %:- (SSucc y) = x %:- y

isZero :: Nat -> Bool
isZero n = if n == Zero then True else False

type family IsZero (n :: Nat) :: Bool where
  IsZero n = If (n :==: Zero) True False

sIsZero :: Sing n -> Sing (IsZero n)
sIsZero n = sIf (n %==% sZero) sTrue sFalse

{-
(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True
-}

type family (a :: Bool) :|| (b :: Bool) :: Bool where
  False :|| x = x
  True :|| x = True

(%:||) :: Sing a -> Sing b -> Sing (a :|| b)
SFalse %:|| x = x
STrue %:|| _ = sTrue

{-
contains :: Eq a => a -> List a -> Bool
contains _ Nil = False
contains elt (Cons h t) = (elt == h) || contains elt t
-}

type family Contains (a :: k) (b :: List k) :: Bool where
  Contains elt Nil = False
  Contains elt (Cons h t) = (elt :==: h) :|| (Contains elt t)

sContains :: forall. SEq (KindParam :: KindIs k) => 
             forall (a :: k). Sing a ->
             forall (list :: List k). Sing list -> Sing (Contains a list)
sContains _ SNil = sFalse
sContains elt (SCons h t) = (elt %==% h) %:|| (sContains elt t)