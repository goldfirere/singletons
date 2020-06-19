module Singletons.LambdasComprehensive where

import Data.Singletons.TH
import Prelude hiding (pred)
import Prelude.Singletons
import Singletons.Nat

$(singletons [d|
 foo :: [Nat]
 foo = map (\x -> either_ pred Succ x) [Left Zero, Right (Succ Zero)]

 -- this is the same as above except that it does not use lambdas
 bar :: [Nat]
 bar = map (either_ pred Succ) [Left Zero, Right (Succ Zero)]
 |])

fooTest1a :: Proxy Foo
fooTest1a = Proxy

fooTest1b :: Proxy [Zero, Succ (Succ Zero)]
fooTest1b = fooTest1a

barTest1a :: Proxy Bar
barTest1a = Proxy

barTest1b :: Proxy [Zero, Succ (Succ Zero)]
barTest1b = barTest1a
