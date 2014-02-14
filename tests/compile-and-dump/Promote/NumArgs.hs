module Promote.NumArgs where

import Data.Singletons.TH
import Singletons.Nat

-- used to test the "num args" feature of promoteDec
-- remove this test once eta-expansion is implemented

$(promote [d|
  returnFunc :: Nat -> Nat -> Nat
  returnFunc _ = Succ
  |])
