{-# OPTIONS_GHC -Wno-unused-imports #-}

module Singletons.Nat where

import Data.Singletons.TH
import Data.Singletons
import Data.Singletons.SuppressUnusedWarnings

-- Work around #190
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Show

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
