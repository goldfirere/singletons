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
             LambdaCase, TemplateHaskell
 #-}

#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE EmptyCase #-}
#endif

module ByHand where

import Prelude hiding (Maybe, Just, Nothing, Either, Left, Right, map, zipWith,
                       (+), (-))
import Unsafe.Coerce
import ByHandAux

#if __GLASGOW_HASKELL__ >= 707
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy
#endif

-----------------------------------
-- Original ADTs ------------------
-----------------------------------

data Nat :: * where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Eq

#if __GLASGOW_HASKELL__ >= 707
-- Kind-level synonyms following singletons naming convention
type a :&& b = a && b
type a :== b = a == b
#else
-- Type-level boolean equality
type family (a :: k) == (b :: k) :: Bool
-- Kind-level synonym
type a :== b = a == b
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

-- Singleton type equality type class
class (kparam ~ 'KProxy) => SEq (kparam :: KProxy k) where
  (%:==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :== b)
  -- omitting definition of %:/=

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
    with_sing_i s' si = unsafeCoerce (MkDI si) s'

withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

class (kparam ~ 'KProxy) => SDecide (kparam :: KProxy k) where
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

type Refuted a = (a -> Void)

data Decision a = Proved a
                | Disproved (Refuted a)

newtype Void = Void Void

-- defunctionalization symbols
data TyFun :: * -> * -> *
data TyCon :: (k1 -> k2) -> (TyFun k1 k2) -> *
type family (f :: TyFun k1 k2 -> *) @@ (x :: k1) :: k2
type instance (TyCon f) @@ x = f x
infixl 9 @@

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
  SZero %:== SZero = STrue
  SZero %:== (SSucc _) = SFalse
  (SSucc _) %:== SZero = SFalse
  (SSucc n) %:== (SSucc n') = n %:== n'

instance SDecide ('KProxy :: KProxy Nat) where
  SZero %~ SZero = Proved Refl
  (SSucc m) %~ (SSucc n) =
    case m %~ n of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  SZero %~ (SSucc _) = Disproved ($emptyLamCase)
  (SSucc _) %~ SZero = Disproved ($emptyLamCase)

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
  SNothing %~ (SJust _) = Disproved ($emptyLamCase)
  (SJust _) %~ SNothing = Disproved ($emptyLamCase)

instance SEq ('KProxy :: KProxy k) => SEq ('KProxy :: KProxy (Maybe k)) where
  SNothing %:== SNothing = STrue
  SNothing %:== (SJust _) = SFalse
  (SJust _) %:== SNothing = SFalse
  (SJust a) %:== (SJust a') = a %:== a'

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
  SNil %:== SNil = STrue
  SNil %:== (SCons _ _) = SFalse
  (SCons _ _) %:== SNil = SFalse
  (SCons a b) %:== (SCons a' b') = (a %:== a') %:&& (b %:== b')

instance SDecide ('KProxy :: KProxy k) => SDecide ('KProxy :: KProxy (List k)) where
  SNil %~ SNil = Proved Refl
  (SCons h1 t1) %~ (SCons h2 t2) =
    case (h1 %~ h2, t1 %~ t2) of
      (Proved Refl, Proved Refl) -> Proved Refl
      (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
      (_, Disproved contra) -> Disproved (\Refl -> contra Refl)
  SNil %~ (SCons _ _) = Disproved ($emptyLamCase)
  (SCons _ _) %~ SNil = Disproved ($emptyLamCase)

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
  SLeft :: forall (a :: k1). Sing a -> Sing (Left a)
  SRight :: forall (b :: k2). Sing b -> Sing (Right b)

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
  (SLeft _) %~ (SRight _) = Disproved ($emptyLamCase)
  (SRight _) %~ (SLeft _) = Disproved ($emptyLamCase)

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
  fromSing = $emptyLamCase
  toSing = $emptyLamCase

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
  SNat %~ (SMaybe {}) = Disproved ($emptyLamCase)
  SNat %~ (SVec {}) = Disproved ($emptyLamCase)
  (SMaybe {}) %~ SNat = Disproved ($emptyLamCase)
  (SMaybe a) %~ (SMaybe b) =
    case a %~ b of
      Proved Refl -> Proved Refl
      Disproved contra -> Disproved (\Refl -> contra Refl)
  (SMaybe {}) %~ (SVec {}) = Disproved ($emptyLamCase)
  (SVec {}) %~ SNat = Disproved ($emptyLamCase)
  (SVec {}) %~ (SMaybe {}) = Disproved ($emptyLamCase)
  (SVec a1 n1) %~ (SVec a2 n2) =
    case (a1 %~ a2, n1 %~ n2) of
      (Proved Refl, Proved Refl) -> Proved Refl
      (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
      (_, Disproved contra) -> Disproved (\Refl -> contra Refl)

#if __GLASGOW_HASKELL__ < 707
type instance (a :: *) == (a :: *) = True
#endif

instance SEq ('KProxy :: KProxy *) where
  a %:== b =
    case a %~ b of
      Proved Refl -> STrue
      Disproved _ -> unsafeCoerce SFalse

-----------------------------------
-- Some example functions ---------
-----------------------------------

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

#if __GLASGOW_HASKELL__ >= 707
type family IsJust (a :: Maybe k) :: Bool where
    IsJust Nothing = False
    IsJust (Just a) = True
#else
type family IsJust (a :: Maybe k) :: Bool
type instance IsJust Nothing = False
type instance IsJust (Just a) = True
#endif

-- defunctionalization symbols
data IsJustSym0 (f :: TyFun (Maybe a) Bool)
type instance (@@) IsJustSym0 a = IsJust a

sIsJust :: Sing a -> Sing (IsJust a)
sIsJust SNothing = SFalse
sIsJust (SJust _) = STrue

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

#if __GLASGOW_HASKELL__ >= 707
type family Pred (a :: Nat) :: Nat where
  Pred Zero = Zero
  Pred (Succ n) = n
#else
type family Pred (a :: Nat) :: Nat
type instance Pred Zero = Zero
type instance Pred (Succ n) = n
#endif

data PredSym0 (a :: TyFun Nat Nat)
type instance (@@) PredSym0 a = Pred a

sPred :: forall (t :: Nat). Sing t -> Sing (Pred t)
sPred SZero = SZero
sPred (SSucc n) = n

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons h t) = Cons (f h) (map f t)

#if __GLASGOW_HASKELL__ >= 707
type family Map (f :: TyFun k1 k2 -> *) (l :: List k1) :: List k2 where
    Map f Nil = Nil
    Map f (Cons h t) = Cons ((@@) f h) (Map f t)
#else
type family Map (f :: TyFun k1 k2 -> *) (l :: List k1) :: List k2
type instance Map f Nil = Nil
type instance Map f (Cons h t) = Cons ((@@) f h) (Map f t)
#endif

-- defunctionalization symbols
data MapSym1 (k1 :: TyFun a b -> *)
             (k2 :: TyFun (List a) (List b))
data MapSym0 (k3 :: TyFun (TyFun a b -> *)
                          (TyFun (List a) (List b) -> *))
type instance (@@) (MapSym1 f) xs = Map f xs
type instance (@@)  MapSym0 f     = MapSym1 f

sMap :: forall (a :: List k1) (f :: TyFun k1 k2 -> *).
       (forall b. Proxy f -> Sing b -> Sing ((@@) f b)) -> Sing a -> Sing (Map f a)
sMap _ SNil = SNil
sMap f (SCons h t) = SCons (f Proxy h) (sMap f t)

-- Alternative implementation of sMap with Proxy outside of callback.
-- Not generated by the library.
sMap2 :: forall (a :: List k1) (f :: TyFun k1 k2 -> *). Proxy f ->
       (forall b. Sing b -> Sing ((@@) f b)) -> Sing a -> Sing (Map f a)
sMap2 _ _ SNil = SNil
sMap2 p f (SCons h t) = SCons (f h) (sMap2 p f t)

-- test sMap
foo :: Sing (Cons (Succ (Succ Zero)) (Cons (Succ Zero) Nil))
foo = sMap (\(_ :: Proxy (TyCon Succ)) -> SSucc) (SCons (SSucc SZero) (SCons SZero SNil))

-- test sMap2
bar :: Sing (Cons (Succ (Succ Zero)) (Cons (Succ Zero) Nil))
bar = sMap2 (Proxy :: Proxy (TyCon Succ)) (SSucc) (SCons (SSucc SZero) (SCons SZero SNil))

baz :: Sing (Cons Zero (Cons Zero Nil))
baz = sMap2 (Proxy :: Proxy PredSym0) (sPred) (SCons (SSucc SZero) (SCons SZero SNil))

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _           = []

#if __GLASGOW_HASKELL__ >= 707
type family ZipWith (k1 :: TyFun a (TyFun b c -> *) -> *)
                    (k2 :: List a)
                    (k3 :: List b) :: List c where
  ZipWith f (Cons x xs) (Cons y ys) = Cons ((@@) ((@@) f x) y) (ZipWith f xs ys)
  ZipWith f xs ys = Nil
#else
type family ZipWith (k1 :: TyFun a (TyFun b c -> *) -> *)
                    (k2 :: List a)
                    (k3 :: List b) :: List c
type instance ZipWith f (Cons x xs) (Cons y ys) = Cons ((@@) ((@@) f x) y) (ZipWith f xs ys)
type instance ZipWith f xs ys = Nil
#endif

data ZipWithSym2 (k1 :: TyFun a (TyFun b c -> *) -> *)
                 (k2 :: List a)
                 (k3 :: TyFun (List b) (List c))
data ZipWithSym1 (k1 :: TyFun a (TyFun b c -> *) -> *)
                 (k2 :: TyFun (List a) (TyFun (List b) (List c) -> *))
data ZipWithSym0 (k3 :: TyFun (TyFun a (TyFun b c -> *) -> *)
                              (TyFun (List a)
                                     (TyFun (List b) (List c) -> *) -> *))
type instance (@@) (ZipWithSym2 f xs) ys = ZipWith f xs ys
type instance (@@) (ZipWithSym1 f)    xs = ZipWithSym2 f xs
type instance (@@)  ZipWithSym0 f        = ZipWithSym1 f

{-
sZipWith :: forall (k1 :: TyFun a (TyFun b c -> *) -> *) (k2 :: List a) (k3 :: List b).
  (forall (t1 :: a). Proxy k1 -> Sing t1 -> forall (t2 :: b). Sing t2 -> Sing ((@@) ((@@) k1 t1) t2))
  -> Sing k2 -> Sing k3 -> Sing (ZipWith k1 k2 k3)
sZipWith f (SCons x xs) (SCons y ys) = SCons (f Proxy x y) (sZipWith f xs ys)
sZipWith _ _ _                       = SNil
-}

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l _ (Left x) = l x
either _ r (Right x) = r x

#if __GLASGOW_HASKELL__ >= 707
type family Either_ (l :: TyFun a c -> *) (r :: TyFun b c -> *) (e :: Either a b) :: c where
    Either_ l r (Left x) = (@@) l x
    Either_ l r (Right x) = (@@) r x
#else
type family Either_ (l :: TyFun a c -> *) (r :: TyFun b c -> *) (e :: Either a b) :: c
type instance Either_ l r (Left x) = (@@) l x
type instance Either_ l r (Right x) = (@@) r x
#endif

-- defunctionalization symbols
data Either_Sym2 (k1 :: TyFun a c -> *)
                 (k2 :: TyFun b c -> *)
                 (k3 :: TyFun (Either a b) c)
data Either_Sym1 (k1 :: TyFun a c -> *)
                 (k2 :: TyFun (TyFun b c -> *)
                              (TyFun (Either a b) c -> *))
data Either_Sym0 (k1 :: TyFun (TyFun a c -> *)
                              (TyFun (TyFun b c -> *)
                                     (TyFun (Either a b) c -> *) -> *))
type instance (@@) (Either_Sym2 k1 k2) k3 = Either_     k1 k2 k3
type instance (@@) (Either_Sym1 k1)    k2 = Either_Sym2 k1 k2
type instance (@@)  Either_Sym0        k1 = Either_Sym1 k1

sEither :: forall (l :: TyFun a c -> *)
                  (r :: TyFun b c -> *)
                  (e :: Either a b).
           (forall n. Proxy l -> Sing n -> Sing ((@@) l n)) ->
           (forall n. Proxy r -> Sing n -> Sing ((@@) r n)) ->
           Sing e -> Sing (Either_ l r e)
sEither l _ (SLeft x) = l Proxy x
sEither _ r (SRight x) = r Proxy x

-- Alternative implementation of sEither with Proxy outside of callbacks.
-- Not generated by the library.
sEither2 :: forall (l :: TyFun a c -> *)
                   (r :: TyFun b c -> *)
                   (e :: Either a b).
           Proxy l -> Proxy r ->
           (forall n. Sing n -> Sing ((@@) l n)) ->
           (forall n. Sing n -> Sing ((@@) r n)) ->
           Sing e -> Sing (Either_ l r e)
sEither2 _ _ l _ (SLeft  x) = l x
sEither2 _ _ _ r (SRight x) = r x

eitherFoo :: Sing (Succ (Succ Zero))
eitherFoo = sEither (\(_ :: Proxy (TyCon Succ)) -> SSucc)
                    (\(_ :: Proxy PredSym0)     -> sPred) (SLeft (SSucc SZero))

eitherBar :: Sing Zero
eitherBar = sEither2 (Proxy :: Proxy (TyCon Succ))
                     (Proxy :: Proxy PredSym0)
                     SSucc
                     sPred (SRight (SSucc SZero))

eitherToNat :: Either Nat Nat -> Nat
eitherToNat (Left  x) = x
eitherToNat (Right x) = x

#if __GLASGOW_HASKELL__ >= 707
type family EitherToNat (e :: Either Nat Nat) :: Nat where
    EitherToNat (Left x) = x
    EitherToNat (Right x) = x
#else
type family EitherToNat (e :: Either Nat Nat) :: Nat
type instance EitherToNat (Left x) = x
type instance EitherToNat (Right x) = x
#endif

sEitherToNat :: Sing a -> Sing (EitherToNat a)
sEitherToNat (SLeft x) = x
sEitherToNat (SRight x) = x

liftMaybe :: (a -> b) -> Maybe a -> Maybe b
liftMaybe _ Nothing = Nothing
liftMaybe f (Just a) = Just (f a)

#if __GLASGOW_HASKELL__ >= 707
type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just ((@@) f a)
#else
type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b
type instance LiftMaybe f Nothing = Nothing
type instance LiftMaybe f (Just a) = Just ((@@) f a)
#endif

data LiftMaybeSym1 (k1 :: TyFun a b -> *)
                   (k2 :: TyFun (Maybe a) (Maybe b))
data LiftMaybeSym0 (k1 :: TyFun (TyFun a b -> *)
                                (TyFun (Maybe a) (Maybe b) -> *))
type instance (@@) (LiftMaybeSym1 k1) k2 = LiftMaybe k1 k2
type instance (@@)  LiftMaybeSym0     k1 = LiftMaybeSym1 k1

sLiftMaybe :: forall (f :: TyFun a b -> *) (x :: Maybe a).
                (forall (y :: a). Proxy f -> Sing y -> Sing ((@@) f y)) ->
                Sing x -> Sing (LiftMaybe f x)
sLiftMaybe _ SNothing = SNothing
sLiftMaybe f (SJust a) = SJust (f Proxy a)

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

-- defunctionalization symbols
data (:+$$) (k1 :: Nat)
            (k2 :: TyFun Nat Nat)
data (:+$)  (k1 :: TyFun Nat (TyFun Nat Nat -> *))
type instance (@@) ((:+$$) k1) k2 = (:+) k1 k2
type instance (@@)  (:+$)  k1     = (:+$$) k1

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

data (:-$$) (k1 :: Nat)
            (k2 :: TyFun Nat Nat)
data (:-$)  (k1 :: TyFun Nat (TyFun Nat Nat -> *))
type instance (@@) ((:-$$) k1) k2 = (:-) k1 k2
type instance (@@)  (:-$)  k1     = (:-$$) k1

(%:-) :: Sing m -> Sing n -> Sing (m :- n)
SZero %:- _ = SZero
(SSucc x) %:- SZero = SSucc x
(SSucc x) %:- (SSucc y) = x %:- y

isZero :: Nat -> Bool
isZero n = if n == Zero then True else False

#if __GLASGOW_HASKELL__ >= 707
type family IsZero (n :: Nat) :: Bool where
  IsZero n = If (n :== Zero) True False
#else
type family IsZero (n :: Nat) :: Bool
type instance IsZero n = If (n :== Zero) True False
#endif

data IsZeroSym0 (a :: TyFun Nat Bool)
type instance (@@) IsZeroSym0 a = IsZero a

sIsZero :: Sing n -> Sing (IsZero n)
sIsZero n = sIf (n %:== SZero) STrue SFalse

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

data (:||$$) (k1 :: Bool)
             (k2 :: TyFun Bool Bool)
data (:||$)  (k1 :: TyFun Bool (TyFun Bool Bool -> *))
type instance (@@) ((:||$$) a) b = (:||) a b
type instance (@@) (:||$) a = (:||$$) a

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
  Contains elt (Cons h t) = (elt :== h) :|| (Contains elt t)
#else
type family Contains (a :: k) (b :: List k) :: Bool
type instance Contains elt Nil = False
type instance Contains elt (Cons h t) = (elt :== h) :|| (Contains elt t)
#endif

data ContainsSym1 (k1 :: a)
                  (k2 :: TyFun (List a) Bool)
data ContainsSym0 (k1 :: TyFun a (TyFun (List a) Bool -> *))
type instance (@@) (ContainsSym1 a) b = Contains a b
type instance (@@)  ContainsSym0 a    = ContainsSym1 a

sContains :: forall. SEq ('KProxy :: KProxy k) =>
             forall (a :: k). Sing a ->
             forall (list :: List k). Sing list -> Sing (Contains a list)
sContains _ SNil = SFalse
sContains elt (SCons h t) = (elt %:== h) %:|| (sContains elt t)

impNat :: forall m n. SingI n => Proxy n -> Sing m -> Sing (n :+ m)
impNat _ sm = (sing :: Sing n) %:+ sm

callImpNat :: forall n m. Sing n -> Sing m -> Sing (n :+ m)
callImpNat sn sm = withSingI sn (impNat (Proxy :: Proxy n) sm)

instance Show (Sing (n :: Nat)) where
  show SZero = "SZero"
  show (SSucc n) = "SSucc (" ++ (show n) ++ ")"
