module Singletons.T145 where

import Data.Singletons.TH
import Data.Kind

$(singletons [d|
  class Column (f :: Type -> Type) where
    col :: f a -> a -> Bool
  |])
