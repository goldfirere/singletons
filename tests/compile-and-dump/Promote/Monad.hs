module Promote.Monad where

import Data.Singletons
import Data.Singletons.TH
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Maybe
import Data.Promotion.Prelude.Monad

$(promote [d|
  data Foo = A | B | C | D

  fooL1 :: [Foo]
  fooL1 = return A
{-
  fooL2 :: [Foo] -> [Foo]
  fooL2 xs = do
    x <- xs
    return x
-}
  fooM1 :: Maybe Foo
  fooM1 = return A

  fooM2 :: Maybe Foo
  fooM2 = fooM1 >>= return
 |])

fooL1a :: Proxy FooL1
fooL1a = Proxy

fooL1b :: Proxy ('[A])
fooL1b = fooL1a

{-
fooL2a :: Proxy (FooL2 '[A, B, C, D])
fooL2a = Proxy

fooL2b :: Proxy ('[A, B, C, D])
fooL2b = fooL2a
-}

fooM1a :: Proxy FooM1
fooM1a = Proxy

fooM1b :: Proxy (Just A)
fooM1b = fooM1a

fooM2a :: Proxy FooM2
fooM2a = Proxy

fooM2b :: Proxy (Just A)
fooM2b = fooM2a

{-
foo1a :: Proxy (Transpose '[ '[A,B], '[C,D] ])
foo1a = Proxy

foo1b :: Proxy ('[ '[A,C], '[B,D] ])
foo1b = foo1a
-}
