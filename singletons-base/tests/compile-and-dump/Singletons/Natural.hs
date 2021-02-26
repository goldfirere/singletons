module Singletons.Natural where

import Data.Singletons.TH
import Numeric.Natural (Natural)
import Prelude.Singletons

$(singletons [d|
  newtype Age = MkAge Natural

  addAge :: Age -> Age -> Age
  addAge (MkAge (x :: Natural)) (MkAge (y :: Natural)) =
    MkAge (x + y :: Natural)
  |])
