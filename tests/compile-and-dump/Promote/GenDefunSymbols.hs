{-# OPTIONS_GHC -Wno-unused-imports #-}

module Promote.GenDefunSymbols where

import Data.Singletons (Apply, TyFun)
import Data.Singletons.Promote
import Data.Singletons.SuppressUnusedWarnings
import GHC.TypeLits hiding (type (*))
import Data.Kind

type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just (Apply f a)

data NatT = Zero | Succ NatT

type a :+ b = a + b

$(genDefunSymbols [ ''LiftMaybe, ''NatT, ''(:+) ])
