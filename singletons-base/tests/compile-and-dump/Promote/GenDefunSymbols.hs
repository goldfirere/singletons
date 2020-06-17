module Promote.GenDefunSymbols where

import Data.Singletons (Apply, type (~>))
import Data.Singletons.TH (genDefunSymbols)
import GHC.TypeLits hiding (type (*))
import Data.Kind (Type)

type family LiftMaybe (f :: a ~> b) (x :: Maybe a) :: Maybe b where
    LiftMaybe f Nothing = Nothing
    LiftMaybe f (Just a) = Just (Apply f a)

data NatT = Zero | Succ NatT

type a :+ b = a + b

$(genDefunSymbols [ ''LiftMaybe, ''NatT, ''(:+) ])
