module Singletons.PolyKindsApp where

import Data.Kind (Type)
import Data.Singletons.TH

$(singletons [d|
  class Cls (a :: k -> Type) where
    fff :: (a :: k -> Type) (b :: k)

  -- instance Cls Proxy where
  --  fff = Proxy
  |])
