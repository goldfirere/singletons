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

  foo8 :: (a, b) -> a
  foo8 (x, y) = (\_ -> x) y
  |])

foo8a :: Proxy (Foo8 '(Int, Char))
foo8a = Proxy

foo8b :: Proxy Int
foo8b = foo8a
