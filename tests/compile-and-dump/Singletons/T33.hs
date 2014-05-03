module Singletons.T33 where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  foo :: (Bool, Bool) -> ()
  foo ~(_, _) = ()
  |])
