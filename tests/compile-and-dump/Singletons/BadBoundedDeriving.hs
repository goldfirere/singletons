module Singletons.BadBoundedDeriving where

import Data.Singletons.TH

$(singletons [d|
  data Foo a = Foo | Bar a deriving (Bounded)
  |])
