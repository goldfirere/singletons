module Singletons.EqInstances where

import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.TH
import Singletons.Empty
import Singletons.Operators

$(singEqInstances [''Foo, ''Empty])
