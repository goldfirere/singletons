module Singletons.DataValues where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Singletons.Nat
import Data.Singletons.SuppressUnusedWarnings

$(singletons [d|
  data Pair a b = Pair a b deriving Show

  pr = Pair (Succ Zero) ([Zero])

  complex = Pair (Pair (Just Zero) Zero) False

  tuple = (False, Just Zero, True)

  aList = [Zero, Succ Zero, Succ (Succ Zero)]

  |])
