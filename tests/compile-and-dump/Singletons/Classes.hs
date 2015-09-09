module Singletons.Classes where

import Prelude hiding (const)
import Singletons.Nat
import Data.Singletons
import Data.Singletons.TH
import Language.Haskell.TH.Desugar
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Eq

$(singletons [d|
  const :: a -> b -> a
  const x _ = x

  class MyOrd a where
    mycompare :: a -> a -> Ordering
    (<=>) :: a -> a -> Ordering
    (<=>) = mycompare
    infix 4 <=>

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
  fooCompare A B = LT
  fooCompare B B = GT
  fooCompare B A = EQ

  instance MyOrd Foo where
    -- test that values in instance definitions are eta-expanded
    mycompare = fooCompare

  data Foo2 = F | G

  instance Eq Foo2 where
    F == F = True
    G == G = True
    F == G = False
    G == F = False
 |])

$(promote [d|
  -- instance with overlaping equations. Tests #56
  instance MyOrd Foo2 where
      F `mycompare` F = EQ
      F `mycompare` _ = LT
      _ `mycompare` _ = GT

  instance Ord Foo2 where
    F `compare` F = EQ
    F `compare` _ = LT
    _ `compare` _ = GT

  |])

-- check promotion across different splices (#55)
$(singletons [d|
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
