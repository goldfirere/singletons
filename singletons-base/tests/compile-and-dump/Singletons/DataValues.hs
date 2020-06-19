module Singletons.DataValues where

import Data.Singletons.TH
import Prelude.Singletons
import Singletons.Nat
import Text.Show.Singletons

$(singletons [d|
  data Pair a b = Pair a b deriving Show

  pr = Pair (Succ Zero) ([Zero])

  complex = Pair (Pair (Just Zero) Zero) False

  tuple = (False, Just Zero, True)

  aList = [Zero, Succ Zero, Succ (Succ Zero)]

  |])
