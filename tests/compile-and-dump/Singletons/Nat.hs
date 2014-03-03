module Singletons.Nat where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
      deriving (Eq, Show, Read)

  plus :: Nat -> Nat -> Nat
  plus Zero m = m
  plus (Succ n) m = Succ (plus n m)

  pred :: Nat -> Nat
  pred Zero = Zero
  pred (Succ n) = n
 |])
