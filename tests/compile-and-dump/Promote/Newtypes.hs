module Promote.Newtypes where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(promote [d|
  data Nat = Zero | Succ Nat deriving (Eq)
  newtype Foo = Foo Nat deriving (Eq)
  newtype Bar = Bar { unBar :: Nat }
 |])
