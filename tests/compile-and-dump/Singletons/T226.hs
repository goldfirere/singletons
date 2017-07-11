module T226 where

import Data.Singletons.TH

$(singletons [d| class a ~> b |])
