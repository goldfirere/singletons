module Promote.Records where

import Data.Singletons.TH

$(promote [d|
  data Record a = MkRecord { field1 :: a
                           , field2 :: Bool }
  |])

data Proxy a = Proxy

foo1a :: Proxy (Field2 (MkRecord 5 True))
foo1a = Proxy

foo1b :: Proxy True
foo1b = foo1a

foo2a :: Proxy (Field1 (MkRecord 5 True))
foo2a = Proxy

foo2b :: Proxy 5
foo2b = foo2a
          
