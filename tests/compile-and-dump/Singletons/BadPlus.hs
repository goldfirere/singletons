module Singletons.BadPlus where

import Data.Singletons.TH
import Singletons.Nat

-- Test whether a declaration without type signature is not singletonized.

$(singletons [d|
   badPlus Zero m = m
   badPlus (Succ n) m = Succ (plus n m)

   -- See Note [No type signature = no proxy table]
   inc :: Nat -> Nat
   inc x = badPlus x (Succ Zero)
 |])
