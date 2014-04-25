{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Promote.GenDefunSymbols where

import Data.Singletons (Apply, TyFun)
import Data.Singletons.Promote
import Data.Singletons.SuppressUnusedWarnings
import GHC.TypeLits

#if __GLASGOW_HASKELL__ >= 707
type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just (Apply f a)
#else
type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b
type instance LiftMaybe f Nothing = Nothing
type instance LiftMaybe f (Just a) = Just (Apply f a)
#endif

data NatT = Zero | Succ NatT

type a :+ b = a + b

$(genDefunSymbols [ ''LiftMaybe, ''NatT, ''(:+) ])
