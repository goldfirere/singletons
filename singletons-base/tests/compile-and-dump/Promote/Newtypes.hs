module Promote.Newtypes where

import Data.Singletons.Base.TH
import Singletons.Nat

$(promote [d|
  newtype Foo = Foo Nat deriving (Eq)
  newtype Bar = Bar { unBar :: Nat }
 |])
