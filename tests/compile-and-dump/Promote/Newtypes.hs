module Promote.Newtypes where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Singletons.Nat

$(promote [d|
  newtype Foo = Foo Nat deriving (Eq)
  newtype Bar = Bar { unBar :: Nat }
 |])
