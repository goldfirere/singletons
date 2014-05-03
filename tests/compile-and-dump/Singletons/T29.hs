module Singletons.T29 where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  foo :: Bool -> Bool
  foo x = not $ x
  |])
