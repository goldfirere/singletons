{-# OPTIONS_GHC -Wno-unused-imports #-}

module Promote.Newtypes where

import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH
import Singletons.Nat

$(promote [d|
  newtype Foo = Foo Nat deriving (Eq)
  newtype Bar = Bar { unBar :: Nat }
 |])
