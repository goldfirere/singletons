module T312 where

import Data.Singletons.TH

$(singletons [d|
  class Foo a where
    bar :: a -> b -> b
    bar _ x = x
  |])
