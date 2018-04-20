module T322 where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  (!) :: Bool -> Bool -> Bool
  (!) = (||)
  infixr 2 !
  |])

f1 :: (False && True :! True) :~: True
f1 = Refl
