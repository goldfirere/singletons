module Promote.PatternMatching where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Singletons.Nat

$(promote [d|
  data Pair a b = Pair a b deriving Show
  pr = Pair (Succ Zero) ([Zero])
  complex = Pair (Pair (Just Zero) Zero) False
  tuple = (False, Just Zero, True)
  aList = [Zero, Succ Zero, Succ (Succ Zero)]
 |])

$(promote [d|
  Pair sz lz = pr
  Pair (Pair jz zz) fls = complex
  (tf, tjz, tt) = tuple
  [_, lsz, (Succ blimy)] = aList

  foo1 :: (a, b) -> a
  foo1 (x, y) = (\_ -> x) y

  foo2 :: (# a, b #) -> a
  foo2 t@(# x, y #) = case t of
                        (# a, b #) -> (\_ -> a) b
  |])

foo1a :: Proxy (Foo1 '(Int, Char))
foo1a = Proxy

foo1b :: Proxy Int
foo1b = foo1a

foo2a :: Proxy (Foo2 '(Int, Char))
foo2a = Proxy

foo2b :: Proxy Int
foo2b = foo2a
