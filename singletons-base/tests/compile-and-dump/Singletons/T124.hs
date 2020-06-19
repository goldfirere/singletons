module Singletons.T124 where

import Data.Singletons.TH
import Prelude.Singletons

$(singletons [d|
  foo :: Bool -> ()
  foo True = ()
  foo False = ()
  |])

bar :: SBool b -> STuple0 (Foo b)
bar b = $(sCases ''Bool [| b |] [| STuple0 |])
