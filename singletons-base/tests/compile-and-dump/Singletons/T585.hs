module T585 where

import Data.Singletons.TH

$(singletons [d|
  konst :: forall a {b}. a -> b -> a
  konst x _ = x
  |])
