{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Promote.GenDefunSymbols where

import Data.Singletons (Apply, TyFun)
import Data.Singletons.Promote
import Data.Singletons.SuppressUnusedWarnings
import GHC.TypeLits hiding (type (*))

#if __GLASGOW_HASKELL__ >= 711
import Data.Kind
#endif

type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just (Apply f a)

data NatT = Zero | Succ NatT

type a :+ b = a + b

$(genDefunSymbols [ ''LiftMaybe, ''NatT, ''(:+) ])
