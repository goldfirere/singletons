module T401 where

import Data.Singletons.TH

$(singletons [d|
  f :: (forall a. a -> a) -> b -> b
  f _ x = x
  |])
