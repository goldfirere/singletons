{- Data/Singletons.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This is the public interface file to the singletons library. Please
see the accompanying README file for more information. Haddock is
not currently compatible with the features used here, so the documentation
is all in the README file and /Dependently typed programming with singletons/,
available at <http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf>
-}

{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, TemplateHaskell,
             DataKinds, PolyKinds, TypeOperators, MultiParamTypeClasses,
             FlexibleContexts, RankNTypes, UndecidableInstances,
             FlexibleInstances, ScopedTypeVariables, CPP
 #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-binds #-}
-- We make unused bindings for (||), (&&), and not.

module Data.Singletons (
  KProxy(..), Sing(..), SingI(..), SingE(..), SingRep, KindOf, Demote,
  Any,
  (:==), (:==:),
  SingInstance(..), SingKind(singInstance),
  sTrue, sFalse, SBool, sNothing, sJust, SMaybe, sLeft, sRight, SEither,
  sTuple0, sTuple2, sTuple3, sTuple4, sTuple5, sTuple6, sTuple7,
  STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,
  Not, sNot, (:&&), (%:&&), (:||), (%:||), (:&&:), (:||:), (:/=), (:/=:),
  SEq((%==%), (%/=%)), (%:==), (%:/=),
  If, sIf, 
  sNil, sCons, SList, Head, Tail,
  cases, bugInGHC,
  genSingletons, singletons, genPromotions, promote,
  promoteEqInstances, promoteEqInstance, singEqInstance, singEqInstances
  ) where

import Data.Singletons.Singletons
import Data.Singletons.Promote
import Language.Haskell.TH
import Data.Singletons.Util
import GHC.Exts
import GHC.TypeLits

#if __GLASGOW_HASKELL__ >= 707
import Data.Proxy
#else
import Data.Singletons.Legacy
#endif

-- Access the kind of a type variable
type KindOf (a :: k) = ('KProxy :: KProxy k)

-- Declarations of singleton structures
data family Sing (a :: k)
class SingI (a :: k) where
  sing :: Sing a
class (kparam ~ 'KProxy) => SingE (kparam :: KProxy k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam

-- SingRep is a synonym for (SingI, SingE)
class    (SingI a, SingE (KindOf a)) => SingRep (a :: k)
instance (SingI a, SingE (KindOf a)) => SingRep (a :: k)

-- Abbreviation for DemoteRep
type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)

data SingInstance (a :: k) where
  SingInstance :: SingRep a => SingInstance a
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  singInstance :: forall (a :: k). Sing a -> SingInstance a

-- type-level conditional
type family If (a :: Bool) (b :: k) (c :: k) :: k
type instance If 'True b c = b
type instance If 'False b c = c

-- some useful singletons
$(genSingletons [''Bool, ''Maybe, ''Either, ''[]])
$(genSingletons [''(), ''(,), ''(,,), ''(,,,), ''(,,,,), ''(,,,,,), ''(,,,,,,)])

-- ... with some functions over Booleans
$(singletons [d|
  not :: Bool -> Bool
  not False = True
  not True  = False

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && a = a

  (||) :: Bool -> Bool -> Bool
  False || a = a
  True  || _ = True
  |])

type family (a :: k) :==: (b :: k) :: Bool
type a :== b = a :==: b -- :== and :==: are synonyms

type a :/=: b = Not (a :==: b)
type a :/= b = a :/=: b

-- the singleton analogue of @Eq@
class (kparam ~ 'KProxy) => SEq (kparam :: KProxy k) where
  (%==%) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :==: b)
  (%/=%) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :/=: b)
  a %/=% b = sNot (a %==% b)

(%:==) :: forall (a :: k) (b :: k). SEq ('KProxy :: KProxy k)
       => Sing a -> Sing b -> Sing (a :==: b)
(%:==) = (%==%)

(%:/=) :: forall (a :: k) (b :: k). SEq ('KProxy :: KProxy k)
       => Sing a -> Sing b -> Sing (a :/=: b)
(%:/=) = (%/=%)

$(singEqInstances [''Bool, ''Maybe, ''Either, ''[]])
$(singEqInstances [''(), ''(,), ''(,,), ''(,,,), ''(,,,,), ''(,,,,,), ''(,,,,,,)])

-- singleton conditional
sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

-- symmetric syntax synonyms
type a :&&: b = a :&& b
type a :||: b = a :|| b

#if __GLASGOW_HASKELL__ >= 707

-- operator on type-level lists
type family Head a where
  Head (h ': t) = h
type family Tail a where
  Tail (h ': t) = t

#else
    
-- operate on type-level lists
type family Head (a :: [k]) :: k
type instance Head (h ': t) = h

type family Tail (a :: [k]) :: [k]
type instance Tail (h ': t) = t

#endif

#if __GLASGOW_HASKELL__ >= 707

-- define singletons for TypeLits

data instance Sing (n :: Nat) where
  SNat :: forall (n :: Nat). SingRep n => Integer -> Sing n
instance KnownNat n => SingI n where
  sing = SNat (natVal (Proxy :: Proxy n))
instance SingE ('KProxy :: KProxy Nat) where
  type DemoteRep ('KProxy :: KProxy Nat) = Integer
  fromSing (SNat n) = n
instance SingKind ('KProxy :: KProxy Nat) where
  singInstance (SNat _) = SingInstance

data instance Sing (n :: Symbol) where
  SSym :: forall (n :: Symbol). SingRep n => String -> Sing n
instance KnownSymbol n => SingI n where
  sing = SSym (symbolVal (Proxy :: Proxy n))
instance SingE ('KProxy :: KProxy Symbol) where
  type DemoteRep ('KProxy :: KProxy Symbol) = String
  fromSing (SSym n) = n
instance SingKind ('KProxy :: KProxy Symbol) where
  singInstance (SSym _) = SingInstance
  
#endif

-- allows creation of a singleton when a proxy is at hand
singByProxy :: SingI a => proxy a -> Sing a
singByProxy _ = sing

-- allows for automatic checking of all constructors in a GADT for instance
-- inference
cases :: Name -> Q Exp -> Q Exp -> Q Exp
cases tyName expq bodyq = do
  info <- reifyWithWarning tyName
  case info of
    TyConI (DataD _ _ _ ctors _) -> buildCases ctors
    TyConI (NewtypeD _ _ _ ctor _) -> buildCases [ctor]
    _ -> fail $ "Using <<cases>> with something other than a type constructor: "
                ++ (show tyName)
  where buildCases :: [Con] -> Q Exp
        buildCases ctors =
          caseE expq (map ((flip (flip match (normalB bodyq)) []) . conToPat) ctors)

        conToPat :: Con -> Q Pat
        conToPat = ctor1Case
          (\name tys -> conP name (replicate (length tys) wildP))

-- useful when suppressing GHC's warnings about incomplete pattern matches
bugInGHC :: forall a. a
bugInGHC = error "Bug encountered in GHC -- this should never happen"
