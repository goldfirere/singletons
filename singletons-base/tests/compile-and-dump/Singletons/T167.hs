module Singletons.T167 where

import Data.Singletons.Base.TH
import GHC.TypeLits

type DiffList = [Bool] -> [Bool]

$(singletonsOnly [d|
  class Foo a where
    foosPrec :: Natural -> a -> DiffList
    fooList  :: a -> DiffList
    fooList = undefined

  instance Foo a => Foo [a] where
    foosPrec _ = fooList
  |])
