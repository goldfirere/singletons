module Singletons.OrdDeriving where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  data Nat = Zero | Succ Nat
    deriving (Eq, Ord)

  data Foo a b c d = A a b c d
                   | B a b c d
                   | C a b c d
                   | D a b c d
                   | E a b c d
                   | F a b c d deriving (Eq,Ord)
  |])

foo1a :: Proxy (Zero < Succ Zero)
foo1a = Proxy

foo1b :: Proxy True
foo1b = foo1a

foo2a :: Proxy (Succ (Succ Zero) `Compare` Zero)
foo2a = Proxy

foo2b :: Proxy GT
foo2b = foo2a

foo3a :: Proxy (A 1 2 3 4 `Compare` A 1 2 3 4)
foo3a = Proxy

foo3b :: Proxy EQ
foo3b = foo3a

foo4a :: Proxy (A 1 2 3 4 `Compare` A 1 2 3 5)
foo4a = Proxy

foo4b :: Proxy LT
foo4b = foo4a

foo5a :: Proxy (A 1 2 3 4 `Compare` A 1 2 3 3)
foo5a = Proxy

foo5b :: Proxy GT
foo5b = foo5a

foo6a :: Proxy (A 1 2 3 4 `Compare` B 1 2 3 4)
foo6a = Proxy

foo6b :: Proxy LT
foo6b = foo6a

foo7a :: Proxy (B 1 2 3 4 `Compare` A 1 2 3 4)
foo7a = Proxy

foo7b :: Proxy GT
foo7b = foo7a
