module Promote.NumArgs where

import Data.Singletons.TH
import Singletons.Nat

-- tests the "num args" feature of promoteDec. The idea is that when clauses of
-- a function have less patterns than required by the type signature the
-- promoted type family should have this fact reflected in its return kind,
-- which should be turned into a series of nested TyFuns (type level functions)

$(promote [d|
  returnFunc :: Nat -> Nat -> Nat
  returnFunc _ = Succ

  -- promotion of two functions below also depends on "num args"
  id :: a -> a
  id x = x

  idFoo :: c -> a -> a
  idFoo _ = id
  |])
