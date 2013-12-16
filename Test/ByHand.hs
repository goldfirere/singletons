{- ByHand.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

Shows the derivations for the singleton definitions done by hand.
This file is a great way to understand the singleton encoding better.

-}

{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, KindSignatures, GADTs,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             RankNTypes, TypeOperators, MultiParamTypeClasses,
             FunctionalDependencies, ScopedTypeVariables, CPP,
             EmptyCase, LambdaCase
 #-}

module Test.ByHand where

import Prelude hiding (Maybe, Just, Nothing, Either, Left, Right, map,
                       (+), (-))
import Unsafe.Coerce

import Data.Singletons.Types
import Data.Singletons.Void

#if __GLASGOW_HASKELL__ >= 707
import Data.Type.Bool
import Data.Type.Equality
#endif

-----------------------------------
-- Original ADTs ------------------
-----------------------------------

data Nat :: * where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Eq

#if __GLASGOW_HASKELL__ >= 707
type a :&& b = a && b
#else
    -- This is necessary for defining boolean equality at the type level
type family (a :: Bool) :&& (b :: Bool) :: Bool
type instance False :&& False = False
type instance False :&& True = False
type instance True :&& False = False
type instance True :&& True = True
#endif

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

#if __GLASGOW_HASKELL__ < 707
-- Type-level boolean equality
type family (a :: k) == (b :: k) :: Bool
#endif

-- Singleton type equality type class
class (t ~ 'KProxy) => SEq (t :: KProxy k) where
  (%==%) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a == b)

-- A way to store instances of SingRep at runtime
data SingInstance :: k -> * where 
  SingInstance :: SingI a => SingInstance a

-- A "kind class" marking those kinds that are considered in our
-- universe. Any type of a kind that has a SingKind instance can
-- be made into a singleton.
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam
  toSing :: DemoteRep kparam -> SomeSing kparam

-- The Sing family
data family Sing (a :: k)

-- Implicit singletons
class SingI (a :: k) where
  sing :: Sing a

-- Convenient abbreviations
type KindOf (a :: k) = ('KProxy :: KProxy k)
type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)

-- Wraps up a singleton
data SomeSing :: KProxy k -> * where
  SomeSing :: Sing (a :: k) -> SomeSing ('KProxy :: KProxy k)

#if __GLASGOW_HASKELL__ < 707
type family If (a :: Bool) (b :: k) (c :: k) :: k
type instance If True b c = b
type instance If False b c = c
#endif

sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

withSomeSing :: SingKind ('KProxy :: KProxy k)
             => DemoteRep ('KProxy :: KProxy k)
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing x f = 
  case toSing x of
    SomeSing x' -> f x'

newtype DontInstantiate a = MkDI { unDI :: SingI a => SingInstance a }

singInstance :: forall (a :: k). Sing a -> SingInstance a
singInstance s = with_sing_i s SingInstance
  where
    with_sing_i :: Sing a -> (SingI a => SingInstance a) -> SingInstance a
    with_sing_i s si = unsafeCoerce (MkDI si) s

withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

class (kparam ~ 'KProxy) => SDecide (kparam :: KProxy k) where
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

-----------------------------------
-- Auto-generated code ------------
-----------------------------------

-- Nat

data instance Sing (a :: Nat) where
  SZero :: Sing Zero
  SSucc :: Sing n -> Sing (Succ n)

#if __GLASGOW_HASKELL__ >= 707

type family EqualsNat (a :: Nat) (b :: Nat) where
  EqualsNat Zero Zero = True
  EqualsNat (Succ a) (Succ b) = a == b
  EqualsNat (n1 :: Nat) (n2 :: Nat) = False
type instance (a :: Nat) == (b :: Nat) = EqualsNat a b

#else

type instance Zero == Zero = True
type instance Zero == (Succ n) = False
type instance (Succ n) == Zero = False
type instance (Succ n) == (Succ n') = n == n'

#endif

instance SEq ('KProxy :: KProxy Nat) where
  SZero %==% SZero = STrue
  SZero %==% (SSucc _) = SFalse
  (SSucc _) %==% SZero = SFalse
  (SSucc n) %==% (SSucc n') = n %==% n'

instance SDecide ('KProxy :: KProxy Nat) where
  SZero %~ SZero = Proved Refl
  (SSucc m) %~ (SSucc n) =
    case m %~ n of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  SZero %~ (SSucc _) = Disproved (\case {})
  (SSucc _) %~ SZero = Disproved (\case {})

instance SingI Zero where
  sing = SZero
instance SingI n => SingI (Succ n) where
  sing = SSucc sing
instance SingKind ('KProxy :: KProxy Nat) where
  type DemoteRep ('KProxy:: KProxy Nat) = Nat
  fromSing SZero = Zero
  fromSing (SSucc n) = Succ (fromSing n)
  toSing Zero = SomeSing SZero
  toSing (Succ n) = withSomeSing n (\n' -> SomeSing $ SSucc n')

-- Bool

data instance Sing (a :: Bool) where
  SFalse :: Sing False
  STrue :: Sing True

(%:&&) :: forall (a :: Bool) (b :: Bool). Sing a -> Sing b -> Sing (a :&& b)
SFalse %:&& SFalse = SFalse
SFalse %:&& STrue = SFalse
STrue %:&& SFalse = SFalse
STrue %:&& STrue = STrue

instance SingI False where
  sing = SFalse
instance SingI True where
  sing = STrue
instance SingKind ('KProxy :: KProxy Bool) where
  type DemoteRep ('KProxy :: KProxy Bool) = Bool
  fromSing SFalse = False
  fromSing STrue = True
  toSing False = SomeSing SFalse
  toSing True  = SomeSing STrue


-- Maybe

data instance Sing (a :: Maybe k) where
  SNothing :: Sing Nothing
  SJust :: forall (a :: k). Sing a -> Sing (Just a)

#if __GLASGOW_HASKELL__ >= 707

type family EqualsMaybe (a :: Maybe k) (b :: Maybe k) where
  EqualsMaybe Nothing Nothing = True
  EqualsMaybe (Just a) (Just a') = a == a'
  EqualsMaybe (x :: Maybe k) (y :: Maybe k) = False
type instance (a :: Maybe k) == (b :: Maybe k) = EqualsMaybe a b

#else

type instance Nothing == Nothing = True
type instance Nothing == (Just a) = False
type instance (Just a) == Nothing = False
type instance (Just a) == (Just a') = a == a'

#endif

instance SDecide ('KProxy :: KProxy k) => SDecide ('KProxy :: KProxy (Maybe k)) where
  SNothing %~ SNothing = Proved Refl
  (SJust x) %~ (SJust y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  SNothing %~ (SJust _) = Disproved (\case {})
  (SJust _) %~ SNothing = Disproved (\case {})

instance SEq ('KProxy :: KProxy k) => SEq ('KProxy :: KProxy (Maybe k)) where
  SNothing %==% SNothing = STrue
  SNothing %==% (SJust _) = SFalse
  (SJust _) %==% SNothing = SFalse
  (SJust a) %==% (SJust a') = a %==% a'

instance SingI (Nothing :: Maybe k) where
  sing = SNothing
instance SingI a => SingI (Just (a :: k)) where
  sing = SJust sing
instance SingKind ('KProxy :: KProxy k) => SingKind ('KProxy :: KProxy (Maybe k)) where
  type DemoteRep ('KProxy :: KProxy (Maybe k)) = Maybe (DemoteRep ('KProxy :: KProxy k))
  fromSing SNothing = Nothing
  fromSing (SJust a) = Just (fromSing a)
  toSing Nothing = SomeSing SNothing
  toSing (Just x) =
    case toSing x :: SomeSing ('KProxy :: KProxy k) of
      SomeSing x' -> SomeSing $ SJust x'

-- List

data instance Sing (a :: List k) where
  SNil :: Sing Nil
  SCons :: forall (h :: k) (t :: List k). Sing h -> Sing t -> Sing (Cons h t)

#if __GLASGOW_HASKELL__ >= 707

type family EqualsList (a :: List k) (b :: List k) where
  EqualsList Nil Nil = True
  EqualsList (Cons a b) (Cons a' b') = (a == a') :&& (b == b')
  EqualsList (x :: List k) (y :: List k) = False
type instance (a :: List k) == (b :: List k) = EqualsList a b

#else

type instance Nil == Nil = True
type instance Nil == (Cons a b) = False
type instance (Cons a b) == Nil = False
type instance (Cons a b) == (Cons a' b') = (a == a') :&& (b == b')

#endif

instance SEq ('KProxy :: KProxy k) => SEq ('KProxy :: KProxy (List k)) where
  SNil %==% SNil = STrue
  SNil %==% (SCons _ _) = SFalse
  (SCons _ _) %==% SNil = SFalse
  (SCons a b) %==% (SCons a' b') = (a %==% a') %:&& (b %==% b')

instance SDecide ('KProxy :: KProxy k) => SDecide ('KProxy :: KProxy (List k)) where
  SNil %~ SNil = Proved Refl
  (SCons h1 t1) %~ (SCons h2 t2) =
    case (h1 %~ h2, t1 %~ t2) of
      (Proved Refl, Proved Refl) -> Proved Refl
      (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
      (_, Disproved contra) -> Disproved (\Refl -> contra Refl)
  SNil %~ (SCons _ _) = Disproved (\case {})
  (SCons _ _) %~ SNil = Disproved (\case {})

instance SingI Nil where
  sing = SNil
instance (SingI h, SingI t) =>
           SingI (Cons (h :: k) (t :: List k)) where
  sing = SCons sing sing
instance SingKind ('KProxy :: KProxy k) => SingKind ('KProxy :: KProxy (List k)) where
  type DemoteRep ('KProxy :: KProxy (List k)) = List (DemoteRep ('KProxy :: KProxy k))
  fromSing SNil = Nil
  fromSing (SCons h t) = Cons (fromSing h) (fromSing t)
  toSing Nil = SomeSing SNil
  toSing (Cons h t) =
    case ( toSing h :: SomeSing ('KProxy :: KProxy k)
         , toSing t :: SomeSing ('KProxy :: KProxy (List k)) ) of
      (SomeSing h', SomeSing t') -> SomeSing $ SCons h' t'

-- Either

data instance Sing (a :: Either k1 k2) where
  SLeft :: forall (a :: k). Sing a -> Sing (Left a)
  SRight :: forall (b :: k). Sing b -> Sing (Right b)

instance (SingI a) => SingI (Left (a :: k)) where
  sing = SLeft sing
instance (SingI b) => SingI (Right (b :: k)) where
  sing = SRight sing
instance (SingKind ('KProxy :: KProxy k1), SingKind ('KProxy :: KProxy k2))
           => SingKind ('KProxy :: KProxy (Either k1 k2)) where
  type DemoteRep ('KProxy :: KProxy (Either k1 k2)) =
    Either (DemoteRep ('KProxy :: KProxy k1)) (DemoteRep ('KProxy :: KProxy k2))
  fromSing (SLeft x) = Left (fromSing x)
  fromSing (SRight x) = Right (fromSing x)
  toSing (Left x) =
    case toSing x :: SomeSing ('KProxy :: KProxy k1) of
      SomeSing x' -> SomeSing $ SLeft x'
  toSing (Right x) =
    case toSing x :: SomeSing ('KProxy :: KProxy k2) of
      SomeSing x' -> SomeSing $ SRight x'

instance (SDecide ('KProxy :: KProxy k1), SDecide ('KProxy :: KProxy k2)) => SDecide ('KProxy :: KProxy (Either k1 k2)) where
  (SLeft x) %~ (SLeft y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SRight x) %~ (SRight y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SLeft _) %~ (SRight _) = Disproved (\case {})
  (SRight _) %~ (SLeft _) = Disproved (\case {})

-- Composite

data Composite :: * -> * -> * where
  MkComp :: Either (Maybe a) b -> Composite a b

data instance Sing (a :: Composite k1 k2) where
  SMkComp :: forall (a :: Either (Maybe k1) k2). Sing a -> Sing (MkComp a)

instance SingI a => SingI (MkComp (a :: Either (Maybe k1) k2)) where
  sing = SMkComp sing
instance (SingKind ('KProxy :: KProxy k1), SingKind ('KProxy :: KProxy k2))
           => SingKind ('KProxy :: KProxy (Composite k1 k2)) where
  type DemoteRep ('KProxy :: KProxy (Composite k1 k2)) =
    Composite (DemoteRep ('KProxy :: KProxy k1)) (DemoteRep ('KProxy :: KProxy k2))
  fromSing (SMkComp x) = MkComp (fromSing x)
  toSing (MkComp x) =
    case toSing x :: SomeSing ('KProxy :: KProxy (Either (Maybe k1) k2)) of
      SomeSing x' -> SomeSing $ SMkComp x'

instance (SDecide ('KProxy :: KProxy k1), SDecide ('KProxy :: KProxy k2)) => SDecide ('KProxy :: KProxy (Composite k1 k2)) where
  (SMkComp x) %~ (SMkComp y) =
    case x %~ y of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)

-- Empty

data Empty
data instance Sing (a :: Empty)
instance SingKind ('KProxy :: KProxy Empty) where
  type DemoteRep ('KProxy :: KProxy Empty) = Empty
  fromSing x = case x of {}
  toSing x = case x of {}

-- *

data Vec :: * -> Nat -> * where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

data Rep = Nat | Maybe Rep | Vec Rep Nat

data instance Sing (a :: *) where
  SNat :: Sing Nat
  SMaybe :: Sing a -> Sing (Maybe a)
  SVec :: Sing a -> Sing n -> Sing (Vec a n)

instance SingI Nat where
  sing = SNat
instance SingI a => SingI (Maybe a) where
  sing = SMaybe sing
instance (SingI a, SingI n) => SingI (Vec a n) where
  sing = SVec sing sing

instance SingKind ('KProxy :: KProxy *) where
  type DemoteRep ('KProxy :: KProxy *) = Rep

  fromSing SNat = Nat
  fromSing (SMaybe a) = Maybe (fromSing a)
  fromSing (SVec a n) = Vec (fromSing a) (fromSing n)
  
  toSing Nat = SomeSing SNat
  toSing (Maybe a) =
    case toSing a :: SomeSing ('KProxy :: KProxy *) of
      SomeSing a' -> SomeSing $ SMaybe a'
  toSing (Vec a n) =
    case ( toSing a :: SomeSing ('KProxy :: KProxy *)
         , toSing n :: SomeSing ('KProxy :: KProxy Nat)) of
      (SomeSing a', SomeSing n') -> SomeSing $ SVec a' n'

instance SDecide ('KProxy :: KProxy *) where
  SNat %~ SNat = Proved Refl
  SNat %~ (SMaybe {}) = Disproved (\case {})
  SNat %~ (SVec {}) = Disproved (\case {})
  (SMaybe {}) %~ SNat = Disproved (\case {})
  (SMaybe a) %~ (SMaybe b) =
    case a %~ b of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SMaybe {}) %~ (SVec {}) = Disproved (\case {})
  (SVec {}) %~ SNat = Disproved (\case {})
  (SVec {}) %~ (SMaybe {}) = Disproved (\case {})
  (SVec a1 n1) %~ (SVec a2 n2) =
    case (a1 %~ a2, n1 %~ n2) of
      (Proved Refl, Proved Refl) -> Proved Refl
      (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
      (_, Disproved contra) -> Disproved (\Refl -> contra Refl)

instance SEq ('KProxy :: KProxy *) where
  a %==% b =
    case a %~ b of
      Proved Refl -> STrue
      Disproved _ -> unsafeCoerce SFalse

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
sIsJust SNothing = SFalse
sIsJust (SJust _) = STrue

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons h t) = Cons (f h) (map f t)

type family Map (f :: k1 -> k2) (l :: List k1) :: List k2
type instance Map f Nil = Nil
type instance Map f (Cons h t) = Cons (f h) (Map f t)

sMap :: forall (a :: List k1) (f :: k1 -> k2).
          SingKind ('KProxy :: KProxy k2) =>
          (forall b. Sing b -> Sing (f b)) -> Sing a -> Sing (Map f a)
sMap _ SNil = SNil
sMap f (SCons h t) = SCons (f h) (sMap f t)

-- test sMap
foo = sMap SSucc (SCons (SSucc SZero) (SCons SZero SNil))

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
                {- SingKind ('KProxy :: KProxy b) => -}
                (forall (y :: a). Sing y -> Sing (f y)) ->
                Sing x -> Sing (LiftMaybe f x)
sLiftMaybe _ SNothing = SNothing
sLiftMaybe f (SJust a) = SJust (f a)

(+) :: Nat -> Nat -> Nat
Zero + x = x
(Succ x) + y = Succ (x + y)

#if __GLASGOW_HASKELL__ >= 707
type family (:+) (m :: Nat) (n :: Nat) :: Nat where
  Zero :+ x = x
  (Succ x) :+ y = Succ (x :+ y)
#else
type family (:+) (m :: Nat) (n :: Nat) :: Nat
type instance Zero :+ x = x
type instance (Succ x) :+ y = Succ (x :+ y)
#endif

(%:+) :: Sing m -> Sing n -> Sing (m :+ n)
SZero %:+ x = x
(SSucc x) %:+ y = SSucc (x %:+ y)

(-) :: Nat -> Nat -> Nat
Zero - _ = Zero
(Succ x) - Zero = Succ x
(Succ x) - (Succ y) = x - y

#if __GLASGOW_HASKELL__ >= 707
type family (:-) (m :: Nat) (n :: Nat) :: Nat where
  Zero :- x = Zero
  (Succ x) :- Zero = Succ x
  (Succ x) :- (Succ y) = x :- y
#else
type family (:-) (m :: Nat) (n :: Nat) :: Nat
type instance Zero :- x = Zero
type instance (Succ x) :- Zero = Succ x
type instance (Succ x) :- (Succ y) = x :- y
#endif

(%:-) :: Sing m -> Sing n -> Sing (m :- n)
SZero %:- _ = SZero
(SSucc x) %:- SZero = SSucc x
(SSucc x) %:- (SSucc y) = x %:- y

isZero :: Nat -> Bool
isZero n = if n == Zero then True else False

#if __GLASGOW_HASKELL__ >= 707
type family IsZero (n :: Nat) :: Bool where
  IsZero n = If (n == Zero) True False
#else
type family IsZero (n :: Nat) :: Bool
type instance IsZero n = If (n == Zero) True False
#endif

sIsZero :: Sing n -> Sing (IsZero n)
sIsZero n = sIf (n %==% SZero) STrue SFalse

{-
(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True
-}

#if __GLASGOW_HASKELL__ >= 707
type family (a :: Bool) :|| (b :: Bool) :: Bool where
  False :|| x = x
  True :|| x = True
#else
type family (a :: Bool) :|| (b :: Bool) :: Bool
type instance False :|| x = x
type instance True :|| x = True
#endif

(%:||) :: Sing a -> Sing b -> Sing (a :|| b)
SFalse %:|| x = x
STrue %:|| _ = STrue

{-
contains :: Eq a => a -> List a -> Bool
contains _ Nil = False
contains elt (Cons h t) = (elt == h) || contains elt t
-}

#if __GLASGOW_HASKELL__ >= 707
type family Contains (a :: k) (b :: List k) :: Bool where
  Contains elt Nil = False
  Contains elt (Cons h t) = (elt == h) :|| (Contains elt t)
#else
type family Contains (a :: k) (b :: List k) :: Bool
type instance Contains elt Nil = False
type instance Contains elt (Cons h t) = (elt == h) :|| (Contains elt t)
#endif

sContains :: forall. SEq ('KProxy :: KProxy k) => 
             forall (a :: k). Sing a ->
             forall (list :: List k). Sing list -> Sing (Contains a list)
sContains _ SNil = SFalse
sContains elt (SCons h t) = (elt %==% h) %:|| (sContains elt t)

impNat :: forall m n. SingI n => Proxy n -> Sing m -> Sing (n :+ m)
impNat _ sm = (sing :: Sing n) %:+ sm

callImpNat :: forall n m. Sing n -> Sing m -> Sing (n :+ m)
callImpNat sn sm = withSingI sn (impNat (Proxy :: Proxy n) sm)

instance Show (Sing (n :: Nat)) where
  show SZero = "SZero"
  show (SSucc n) = "SSucc (" ++ (show n) ++ ")"