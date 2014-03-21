module Singletons.ReturnFunc where

import Data.Singletons.TH

$(singletons [d|
  data Nat = Zero | Succ Nat
  returnFunc :: Nat -> Nat -> Nat
  returnFunc _ = Succ
  |])
