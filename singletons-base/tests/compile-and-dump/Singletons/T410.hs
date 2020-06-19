module T410 where

import Data.Bool.Singletons
import Data.Singletons
import Data.Singletons.TH (promote)

$(promote [d|
  class Eq a where
    equals :: a -> a -> Bool
  instance Eq () where
    equals () () = True
  |])
