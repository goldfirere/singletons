module T410 where

import Data.Singletons
import Data.Singletons.Prelude.Bool
import Data.Singletons.TH (promote)

$(promote [d|
  class Eq a where
    equals :: a -> a -> Bool
  instance Eq () where
    equals () () = True
  |])
