{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Singletons.CaseExpressions where

import Data.Singletons.TH
import Data.Singletons.Maybe
import Data.Singletons.Prelude

$(singletons [d|
  foo1 :: a -> Maybe a -> a
  foo1 d x = case x of
               Just y  -> y
               Nothing -> d

  foo2 :: a -> Maybe a -> a
  foo2 d _ = case (Just d) of
               Just y  -> y
--               Nothing -> d
-- the above line causes an "inaccessible code" error. w00t.

  foo3 :: a -> b -> a
  foo3 a b = case (a, b) of
               (p, _)  -> p


  foo4 :: forall a. a -> a
  foo4 x = case x of
             y -> let z :: a
                      z = y
                  in z

  foo5 :: a -> a
  foo5 x = case x of
             y -> (\_ -> x) y
 |])

foo1a :: Proxy (Foo1 Int (Just Char))
foo1a = Proxy

foo1b :: Proxy Char
foo1b = foo1a

foo2a :: Proxy (Foo2 Char Nothing)
foo2a = Proxy

foo2b :: Proxy Char
foo2b = foo2a

foo3a :: Proxy (Foo3 Int Char)
foo3a = Proxy

foo3b :: Proxy Int
foo3b = foo3a

foo4a :: Proxy (Foo4 Int)
foo4a = Proxy

foo4b :: Proxy Int
foo4b = foo4a

foo5a :: Proxy (Foo5 Int)
foo5a = Proxy

foo5b :: Proxy Int
foo5b = foo5a
