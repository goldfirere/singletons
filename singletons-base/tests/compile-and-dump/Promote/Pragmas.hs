module Promote.Pragmas where

import Data.Singletons.TH
import Prelude.Singletons

$(promote [d|
  {-# INLINE foo #-}
  foo :: Bool
  foo = True
 |])
