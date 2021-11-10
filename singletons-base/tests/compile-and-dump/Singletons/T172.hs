module T172 where

import Data.Singletons.TH
import GHC.TypeLits.Singletons
import Prelude.Singletons

$(singletonsOnly [d|
  ($>) :: Natural -> Natural -> Natural
  ($>) = (+)
  |])
