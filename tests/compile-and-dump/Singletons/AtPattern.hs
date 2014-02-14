module Singletons.AtPattern where

import Data.Singletons.TH
import Data.Singletons.Maybe
import Singletons.Nat

$(singletons [d|
  maybePlus :: Maybe Nat -> Maybe Nat
  maybePlus (Just n) = Just (plus (Succ Zero) n)
  maybePlus foo@Nothing = foo
 |])
