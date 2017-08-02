module T163 where

import Data.Singletons.TH

$(singletons [d| data a + b = L a | R b |])
