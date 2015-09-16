module Singletons.T124 where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  foo :: Bool -> ()
  foo True = ()
  foo False = ()
  |])

bar :: SBool b -> STuple0 (Foo b)
bar b = $(sCases ''Bool [| b |] [| STuple0 |])
