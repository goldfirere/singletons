module Promote.OrdDeriving where

import Data.Promotion.Prelude
import Data.Promotion.TH

$(promote [d|
  data Nat = Zero | Succ Nat
    deriving (Eq, Ord)
  |])

foo1a :: Proxy (Zero :< Succ Zero)
foo1a = Proxy

foo1b :: Proxy True
foo1b = foo1a

foo2a :: Proxy (Succ (Succ Zero) `Compare` Zero)
foo2a = Proxy

foo2b :: Proxy GT
foo2b = foo2a
