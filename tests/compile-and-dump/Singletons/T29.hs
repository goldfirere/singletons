module Singletons.T29 where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  foo :: Bool -> Bool
  foo x = not $ x

  -- test that $ works with function composition
  bar :: Bool -> Bool
  bar x = not . not . not $ x
  |])

foo1a :: Proxy (Foo True)
foo1a = Proxy

foo1b :: Proxy False
foo1b = foo1b

bar1a :: Proxy (Bar True)
bar1a = Proxy

bar1b :: Proxy False
bar1b = bar1b
