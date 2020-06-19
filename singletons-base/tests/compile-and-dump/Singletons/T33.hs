module Singletons.T33 where

import Data.Singletons.TH
import Prelude.Singletons

$(singletons [d|
  foo :: (Bool, Bool) -> ()
  foo ~(_, _) = ()
  |])
