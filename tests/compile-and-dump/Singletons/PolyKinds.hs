module Singletons.PolyKinds where

import Data.Singletons.TH

$(singletons [d|
  class Cls (a :: k) where
    fff :: Proxy (a :: k) -> ()
  |])
