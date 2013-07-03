{- Data/Singletons/Exports.hs

(c) Richard Eienberg 2013
eir@cis.upenn.edu

This file contains the fundamental datatype definitions for the singletons
package. These are all re-exported in Data/Singletons.hs
-}

{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, RankNTypes, FlexibleContexts,
             FlexibleInstances, UndecidableInstances, TypeOperators, GADTs,
             CPP #-}

module Data.Singletons.Exports (
  KindIs(..), Sing, SingI(..), SingE(..), SingRep, KindOf, Demote,

  SingInstance(..), SingKind(..), If, Head, Tail
  ) where

#if __GLASGOW_HASKELL__ >= 707

import GHC.TypeLits ( KindIs(..), Sing, SingI(..), SingE(..),
                      SingRep, KindOf, Demote )

#else

-- Kind-level proxy
data KindIs (k :: *) = KindParam

-- Access the kind of a type variable
type KindOf (a :: k) = (KindParam :: KindIs k)

-- Declarations of singleton structures
data family Sing (a :: k)
class SingI (a :: k) where
  sing :: Sing a
class (kparam ~ KindParam) => SingE (kparam :: KindIs k) where
  type DemoteRep kparam :: *
  fromSing :: Sing (a :: k) -> DemoteRep kparam

-- SingRep is a synonym for (SingI, SingE)
class    (SingI a, SingE (KindOf a)) => SingRep (a :: k)
instance (SingI a, SingE (KindOf a)) => SingRep (a :: k)

-- Abbreviation for DemoteRep
type Demote (a :: k) = DemoteRep (KindParam :: KindIs k)

#endif

data SingInstance (a :: k) where
  SingInstance :: SingRep a => SingInstance a
class (kparam ~ KindParam) => SingKind (kparam :: KindIs k) where
  singInstance :: forall (a :: k). Sing a -> SingInstance a

-- type-level conditional
type family If (a :: Bool) (b :: k) (c :: k) :: k
type instance If 'True b c = b
type instance If 'False b c = c

-- operate on type-level lists
type family Head (a :: [k]) :: k
type instance Head (h ': t) = h

type family Tail (a :: [k]) :: [k]
type instance Tail (h ': t) = t
