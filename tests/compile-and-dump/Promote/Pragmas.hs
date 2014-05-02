module Promote.Pragmas where

import Data.Singletons.TH
import Data.Promotion.Prelude

$(promote [d|
  {-# INLINE foo #-}
  foo :: Bool
  foo = True
 |])
