module SingletonsBug where

import Data.Singletons.Base.TH
import GHC.TypeLits

$(singletonsOnly [d|
  class Foo a where
    foosPrec :: Natural -> a -> [Bool] -> [Bool]
    foo      :: a -> [Bool]

    foo        x s = foosPrec 0 x s
  |])
