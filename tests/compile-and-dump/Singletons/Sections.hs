module Singletons.Sections where

import Data.Singletons.TH
import Data.Singletons.List
import Singletons.Nat

$(singletons [d|
  (+) :: Nat -> Nat -> Nat
  Zero + m = m
  (Succ n) + m = Succ (n + m)

  foo1 :: [Nat]
  foo1 = map ((Succ Zero)+) [Zero, Succ Zero]

  foo2 :: [Nat]
  foo2 = map (+(Succ Zero)) [Zero, Succ Zero]

  foo3 :: [Nat]
  foo3 = zipWith (+) [Succ Zero, Succ Zero] [Zero, Succ Zero]
 |])

data Proxy a = Proxy

foo1a :: Proxy Foo1
foo1a = Proxy

foo1b :: Proxy [Succ Zero, Succ (Succ Zero)]
foo1b = foo1a

foo2a :: Proxy Foo2
foo2a = Proxy

foo2b :: Proxy [Succ Zero, Succ (Succ Zero)]
foo2b = foo2a

foo3a :: Proxy Foo3
foo3a = Proxy

foo3b :: Proxy [Succ Zero, Succ (Succ Zero)]
foo3b = foo3a
