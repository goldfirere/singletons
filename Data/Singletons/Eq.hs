{- Data/Singletons/Eq.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Defines the SEq singleton version of the Eq type class.
-}

{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs #-}

module Data.Singletons.Eq (
  type (==), (:==), (:==:), type (/=), (:/=:), (:/=),
  SEq(..), (%:==), (%:/=)
  ) where

import Data.Singletons.Bool
import Data.Singletons.Singletons
import Data.Singletons.Core
import Data.Singletons.Types

type family (a :: k) :==: (b :: k) :: Bool
type a :==  b = a :==: b
type a ==   b = a :==: b

type a :/=: b = Not (a :==: b)
type a :/=  b = a :/=: b
type a /=   b = a :/=: b

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
