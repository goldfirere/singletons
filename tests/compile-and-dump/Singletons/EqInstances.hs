module Singletons.EqInstances where

import Data.Singletons.TH
import Data.Singletons.Bool
import Singletons.Empty
import Singletons.Operators

$(singEqInstances [''Foo, ''Empty])
