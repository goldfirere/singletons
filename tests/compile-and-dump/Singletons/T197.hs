module T197 where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  infixl 5 $$:
  ($$:) :: Bool -> Bool -> Bool
  _ $$: _ = False
 |])
