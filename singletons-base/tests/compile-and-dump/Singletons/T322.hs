module T322 where

import Data.Singletons.TH
import Prelude.Singletons

$(singletons [d|
  (!) :: Bool -> Bool -> Bool
  (!) = (||)
  infixr 2 !
  |])

f1 :: (False && True ! True) :~: True
f1 = Refl
