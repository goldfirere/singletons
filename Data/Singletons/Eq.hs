{- Data/Singletons/Eq.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Defines the SEq singleton version of the Eq type class.
-}

{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs, CPP #-}

module Data.Singletons.Eq (
  type (==), (:==), (:/=),
  SEq(..)
  ) where

import Data.Singletons.Util
import Data.Singletons.Bool
import Data.Singletons.Singletons
import Data.Singletons.Core
import Data.Singletons.Types

#if __GLASGOW_HASKELL__ >= 707

import Data.Type.Equality

type a :== b = a == b

#else

import Data.Singletons.Promote

type family (a :: k) :== (b :: k) :: Bool
type a == b = a :== b

#endif

type a :/= b = Not (a :== b)

-- the singleton analogue of @Eq@
class (kparam ~ 'KProxy) => SEq (kparam :: KProxy k) where
  (%:==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :== b)
  (%:/=) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :/= b)
  a %:/= b = sNot (a %:== b)

#if __GLASGOW_HASKELL__ < 707
$(promoteEqInstances basicTypes)
#endif
       
$(singEqInstancesOnly basicTypes)
