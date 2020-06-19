module T197 where

import Data.Singletons.TH
import Prelude.Singletons

$(singletons [d|
  infixl 5 $$:
  ($$:) :: Bool -> Bool -> Bool
  _ $$: _ = False
 |])
