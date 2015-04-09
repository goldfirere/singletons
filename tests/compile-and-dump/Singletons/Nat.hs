{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Singletons.Nat where

import Data.Singletons.TH
import Data.Singletons
import Data.Proxy
import Data.Singletons.SuppressUnusedWarnings

$(singletons [d|
  data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
      deriving (Eq, Show, Read)
 |])

$(singletons [d|
  plus :: Nat -> Nat -> Nat
  plus Zero m = m
  plus (Succ n) m = Succ (plus n m)

  pred :: Nat -> Nat
  pred Zero = Zero
  pred (Succ n) = n
 |])
