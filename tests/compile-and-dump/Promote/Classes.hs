module Promote.Classes where

import Prelude hiding (Ord(..), const)
import Singletons.Nat
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude.Ord (EQSym0, LTSym0, GTSym0)

$(promote [d|
  const :: a -> b -> a
  const x _ = x

  class MyOrd a where
    mycompare :: a -> a -> Ordering
    (<=>) :: a -> a -> Ordering
    (<=>) = mycompare
--    infix 4 <=>  infix decls don't work due to #65

  instance MyOrd Nat where
    Zero `mycompare` Zero = EQ
    Zero `mycompare` (Succ _) = LT
    (Succ _) `mycompare` Zero = GT
    (Succ n) `mycompare` (Succ m) = m `mycompare` n

    -- test eta-expansion
  instance MyOrd () where
    mycompare _ = const EQ

  data Foo = A | B

  fooCompare :: Foo -> Foo -> Ordering
  fooCompare A A = EQ
  fooCompare A _ = LT
  fooCompare _ _ = GT

  instance MyOrd Foo where
    -- test that values in instance definitions are eta-expanded
    mycompare = fooCompare

  data Foo2 = F | G

  -- instance with overlaping equations. Tests #56
  instance MyOrd Foo2 where
      F `mycompare` F = EQ
      F `mycompare` a = LT
 |])

-- check promotion across different splices (#55)
$(promote [d|
  data Nat' = Zero' | Succ' Nat'
  instance MyOrd Nat' where
    Zero' `mycompare` Zero' = EQ
    Zero' `mycompare` (Succ' _) = LT
    (Succ' _) `mycompare` Zero' = GT
    (Succ' n) `mycompare` (Succ' m) = m `mycompare` n
 |])

foo1a :: Proxy (Zero `Mycompare` (Succ Zero))
foo1a = Proxy

foo1b :: Proxy LT
foo1b = foo1a

foo2a :: Proxy (A `Mycompare` A)
foo2a = Proxy

foo2b :: Proxy EQ
foo2b = foo2a

foo3a :: Proxy ('() `Mycompare` '())
foo3a = Proxy

foo3b :: Proxy EQ
foo3b = foo3a

foo4a :: Proxy (Succ' Zero' :<=> Zero')
foo4a = Proxy

foo4b :: Proxy GT
foo4b = foo4a
