module T613 where

import Data.Proxy
import Data.Proxy.Singletons
import Data.Singletons.TH

$(promote [d|
  f :: forall k (a :: k). Proxy a -> Proxy a
  f x = y
    where
      y = x
  |])
