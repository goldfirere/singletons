module Promote.Classes where

import Prelude hiding (Ord(..), const)
import Singletons.Nat
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude (EQSym0, LTSym0, GTSym0)

$(promote [d|
  const :: a -> b -> a
  const x _ = x

  class Ord a where
    compare :: a -> a -> Ordering

  instance Ord Nat where
    Zero `compare` Zero = EQ
    Zero `compare` (Succ _) = LT
    (Succ _) `compare` Zero = GT
    (Succ n) `compare` (Succ m) = m `compare` n

    -- test eta-expansion
  instance Ord () where
    compare _ = const EQ

  |])

foo1a :: Proxy (Zero `Compare` (Succ Zero))
foo1a = Proxy

foo1b :: Proxy LT
foo1b = foo1a

foo2a :: Proxy ('() `Compare` '())
foo2a = Proxy

foo2b :: Proxy EQ
foo2b = foo2a
