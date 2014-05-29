module T66 where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  data Record a = MkRecord { field1 :: a
                           , field2 :: Bool }
  |])

$(singletons [d|
  neg :: Record a -> Record a
  neg rec@(MkRecord { field1 = _, field2 = b } ) = rec {field2 = not b}
 |])
