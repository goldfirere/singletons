module Promote.CaseExpressions where

import Data.Singletons.TH
import Data.Singletons.Maybe

$(promote [d|
  foo1 :: a -> Maybe a -> a
  foo1 d x = case x of
               Just y  -> y
               Nothing -> d

  foo2 :: a -> Maybe a -> a
  foo2 d x = case (Just d) of
               Just y  -> y
               Nothing -> d

  foo3 :: a -> b -> a
  foo3 a b = case (a, b) of
               (p, _)  -> p
 |])

data Proxy a = Proxy

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
