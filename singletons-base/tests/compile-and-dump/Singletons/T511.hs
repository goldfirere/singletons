module T511 where

import Data.Proxy
import Data.Proxy.Singletons
import Data.Singletons.TH

$(singletons [d|
  data Foo = MkFoo (Proxy (->))
  |])
