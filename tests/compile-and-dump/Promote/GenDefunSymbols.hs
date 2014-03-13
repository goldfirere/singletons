module Promote.GenDefunSymbols where

import Data.Singletons.Promote
import Data.Singletons (type (@@), TyFun)

#if __GLASGOW_HASKELL__ >= 707
type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just ((@@) f a)
#else
type family LiftMaybe (f :: TyFun a b -> *) (x :: Maybe a) :: Maybe b
type instance LiftMaybe f Nothing = Nothing
type instance LiftMaybe f (Just a) = Just ((@@) f a)
#endif

data Nat = Zero | Succ Nat

$(genDefunSymbols [ ''LiftMaybe, ''Nat ])
