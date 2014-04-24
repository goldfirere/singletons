{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Singletons.Operators where

import Data.Proxy
import Data.Singletons
import Data.Singletons.TH
import Singletons.Nat
import Data.Singletons.SuppressUnusedWarnings

$(singletons [d|
  data Foo where
    FLeaf :: Foo
    (:+:) :: Foo -> Foo -> Foo

  child :: Foo -> Foo
  child FLeaf = FLeaf
  child (a :+: _) = a

  (+) :: Nat -> Nat -> Nat
  Zero + m = m
  (Succ n) + m = Succ (n + m)
 |])
