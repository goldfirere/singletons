module Singletons.EqInstances where

import Data.Bool.Singletons
import Data.Singletons.Base.TH
import Singletons.Empty
import Singletons.Operators

$(singEqInstances [''Foo, ''Empty])
