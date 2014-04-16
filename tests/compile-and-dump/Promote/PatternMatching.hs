{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  lsz :: Nat
  fls :: Bool
#if __GLASGOW_HASKELL__ < 707
  blimy :: Nat   -- this is necessary to promote nested patterns
#endif

  foo1 :: (a, b) -> a
  foo1 (x, y) = (\_ -> x) y

  foo2 :: (# a, b #) -> a
  foo2 t@(# x, y #) = case t of
                        (# a, b #) -> (\_ -> a) b
  |])

test1 :: Proxy (Foo1 '(Int, Char)) -> Proxy Int
test1 = id

test2 :: Proxy (Foo2 '(Int, Char)) -> Proxy Int
test2 = id

test3 :: Proxy Lsz -> Proxy (Succ Zero)
test3 = id

test4 :: Proxy Blimy -> Proxy (Succ Zero)
test4 = id

test5 :: Proxy Fls -> Proxy False
test5 = id

