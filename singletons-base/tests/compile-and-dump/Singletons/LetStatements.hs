{-# OPTIONS_GHC -Wno-unused-binds   -Wno-unused-matches
                -Wno-name-shadowing #-}

module Singletons.LetStatements where

import Data.Singletons
import Data.Singletons.TH
import Prelude.Singletons
import Singletons.Nat

$(singletons [d|
  -- type signature required for a constant
  foo1 :: Nat -> Nat
  foo1 x = let y :: Nat
               y = Succ Zero
           in  y

  -- nothing in scope, no type signatures required
  foo2 :: Nat
  foo2 = let y = Succ Zero
             z = Succ y
         in z

  -- using in-scope variable
  foo3 :: Nat -> Nat
  foo3 x = let y :: Nat
               y = Succ x
           in y

  -- passing in-scope variable to a function. Tests also adding in-scope binders
  -- at the call site of f
  foo4 :: Nat -> Nat
  foo4 x = let f :: Nat -> Nat
               f y = Succ y
           in  f x

  -- nested lets, version 1. This could potentially be problematic.
  foo5 :: Nat -> Nat
  foo5 x = let f :: Nat -> Nat
               f y = let z :: Nat
                         z = Succ y
                     in Succ z
           in  f x

  -- nested lets, version 2. This shouldn't cause any problems, so that's just a
  -- sanity check.
  foo6 :: Nat -> Nat
  foo6 x = let f :: Nat -> Nat
               f y = Succ y
           in let z :: Nat
                  z = f x
              in z

  -- name shadowing
  foo7 :: Nat -> Nat
  foo7 x = let x :: Nat
               x = Zero
           in x

  -- lambda binder in let shadows pattern-bound variable
  foo8 :: Nat -> Nat
  foo8 x = let z :: Nat
               z = (\x -> x) Zero
           in z

  -- let-declaring lambdas
  foo9 :: Nat -> Nat
  foo9 x = let z :: Nat -> Nat
               z = (\x -> x)
           in z x
  -- infix declaration
  foo10 :: Nat -> Nat
  foo10 x = let (+) :: Nat -> Nat -> Nat
                Zero     + m = m
                (Succ n) + m = Succ (n + m)
            in (Succ Zero) + x

  -- infix call uses let-bound binder
  foo11 :: Nat -> Nat
  foo11 x = let (+) :: Nat -> Nat -> Nat
                Zero     + m = m
                (Succ n) + m = Succ (n + m)
                z :: Nat
                z = x
            in (Succ Zero) + z

  -- infix let-declaration uses in-scope variable
  foo12 :: Nat -> Nat
  foo12 x = let (+) :: Nat -> Nat -> Nat
                Zero     + m = m
                (Succ n) + m = Succ (n + x)
            in x + (Succ (Succ Zero))

  -- make sure that calls to functions declared outside of let don't receive
  -- extra parameters with in-scope bindings. See #18.
  foo13 :: forall a. a -> a
  foo13 x = let bar :: a
                bar = x
            in foo13_ bar

  foo13_ :: a -> a
  foo13_ y = y

  -- tuple patterns in let statements. See #20
  foo14 :: Nat -> (Nat, Nat)
  foo14 x = let (y, z) = (Succ x, x)
            in  (z, y)
 |])

foo1a :: Proxy (Foo1 Zero)
foo1a = Proxy

foo1b :: Proxy (Succ Zero)
foo1b = foo1a

foo2a :: Proxy Foo2
foo2a = Proxy

foo2b :: Proxy (Succ (Succ Zero))
foo2b = foo2a

foo3a :: Proxy (Foo3 (Succ Zero))
foo3a = Proxy

foo3b :: Proxy (Succ (Succ Zero))
foo3b = foo3a

foo4a :: Proxy (Foo4 (Succ Zero))
foo4a = Proxy

foo4b :: Proxy (Succ (Succ Zero))
foo4b = foo4a

foo5a :: Proxy (Foo5 Zero)
foo5a = Proxy

foo5b :: Proxy (Succ (Succ Zero))
foo5b = foo5a

foo6a :: Proxy (Foo6 Zero)
foo6a = Proxy

foo6b :: Proxy (Succ Zero)
foo6b = foo6a

foo7a :: Proxy (Foo7 (Succ (Succ Zero)))
foo7a = Proxy

foo7b :: Proxy Zero
foo7b = foo7a

foo8a :: Proxy (Foo8 (Succ (Succ Zero)))
foo8a = Proxy

foo8b :: Proxy Zero
foo8b = foo8a

foo9a :: Proxy (Foo9 (Succ (Succ Zero)))
foo9a = Proxy

foo9b :: Proxy (Succ (Succ Zero))
foo9b = foo9a

foo10a :: Proxy (Foo10 (Succ (Succ Zero)))
foo10a = Proxy

foo10b :: Proxy (Succ (Succ (Succ Zero)))
foo10b = foo10a

foo11a :: Proxy (Foo11 (Succ (Succ Zero)))
foo11a = Proxy

foo11b :: Proxy (Succ (Succ (Succ Zero)))
foo11b = foo11a

foo12a :: Proxy (Foo12 (Succ (Succ (Succ Zero))))
foo12a = Proxy

foo12b :: Proxy (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
foo12b = foo12a

foo13a :: Proxy (Foo13 Zero)
foo13a = Proxy

foo13b :: Proxy Zero
foo13b = foo13a

foo14a :: Proxy (Foo14 Zero)
foo14a = Proxy

foo14b :: Proxy '(Zero, Succ Zero)
foo14b = foo14a
