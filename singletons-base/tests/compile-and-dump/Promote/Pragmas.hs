module Promote.Pragmas where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(promote [d|
  {-# INLINE foo #-}
  foo :: Bool
  foo = True
 |])
