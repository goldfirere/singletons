{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-unused-imports #-}

{-# LANGUAGE UnboxedTuples #-}
-- We expect unused binds and name shadowing in foo5 test.
module Singletons.Lambdas where

import Data.Proxy
import Data.Singletons
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH

$(singletons [d|
  -- nothing in scope
  foo0 :: a -> b -> a
  foo0 = (\x y -> x)

  -- eta-reduced function
  foo1 :: a -> b -> a
  foo1 x = (\_ -> x)

  -- same as before, but without eta-reduction
  foo2 :: a -> b -> a
  foo2 x y = (\_ -> x) y

  foo3 :: a -> a
  foo3 x = (\y -> y) x

  -- more lambda parameters + returning in-scope variable
  foo4 :: a -> b -> c -> a
  foo4 x y z = (\_ _ -> x) y z

  -- name shadowing
  -- Note: due to -dsuppress-uniques output of this test does not really
  -- prove that the result is correct. Compiling this file manually and
  -- examining dumped splise of relevant Lamdba reveals that indeed that Lambda
  -- returns its last parameter (ie. y passed in a call) rather than the
  -- first one (ie. x that is shadowed by the binder in a lambda).
  foo5 :: a -> b -> b
  foo5 x y = (\x -> x) y

  -- nested lambdas
  foo6 :: a -> b -> a
  foo6 a b = (\x -> \_ -> x) a b

  -- tuple patterns
  foo7 :: a -> b -> b
  foo7 x y = (\(_, b) -> b) (x, y)

  -- constructor patters=ns
  data Foo a b = Foo a b
  foo8 :: Foo a b -> a
  foo8 x = (\(Foo a _) -> a) x
 |])

foo1a :: Proxy (Foo1 Int Char)
foo1a = Proxy

foo1b :: Proxy Int
foo1b = foo1a

foo2a :: Proxy (Foo2 Int Char)
foo2a = Proxy

foo2b :: Proxy Int
foo2b = foo2a

foo3a :: Proxy (Foo3 Int)
foo3a = Proxy

foo3b :: Proxy Int
foo3b = foo3a

foo4a :: Proxy (Foo4 Int Char Bool)
foo4a = Proxy

foo4b :: Proxy Int
foo4b = foo4a

foo5a :: Proxy (Foo5 Int Bool)
foo5a = Proxy

foo5b :: Proxy Bool
foo5b = foo5a

foo6a :: Proxy (Foo6 Int Char)
foo6a = Proxy

foo6b :: Proxy Int
foo6b = foo6a

foo7a :: Proxy (Foo7 Int Char)
foo7a = Proxy

foo7b :: Proxy Char
foo7b = foo7a
