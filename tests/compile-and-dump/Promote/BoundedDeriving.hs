module Promote.BoundedDeriving where

import Data.Promotion.Prelude
import Data.Promotion.TH

$(promote [d|
  data Foo1 = Foo1 deriving (Bounded)
  data Foo2 = A | B | C | D | E deriving (Bounded)
  data Foo3 a = Foo3 a deriving (Bounded)
  |])

foo1a :: Proxy (MinBound :: Foo1)
foo1a = Proxy

foo1b :: Proxy 'Foo1
foo1b = foo1a

foo1c :: Proxy (MaxBound :: Foo1)
foo1c = Proxy

foo1d :: Proxy 'Foo1
foo1d = foo1c

foo2a :: Proxy (MinBound :: Foo2)
foo2a = Proxy

foo2b :: Proxy 'A
foo2b = foo2a

foo2c :: Proxy (MaxBound :: Foo2)
foo2c = Proxy

foo2d :: Proxy 'E
foo2d = foo2c

foo3a :: Proxy (MinBound :: Foo3 Bool)
foo3a = Proxy

foo3b :: Proxy ('Foo3 False)
foo3b = foo3a

foo3c :: Proxy (MaxBound :: Foo3 Bool)
foo3c = Proxy

foo3d :: Proxy ('Foo3 True)
foo3d = foo3c
