module T297 where

import Data.Kind
import Data.Singletons.TH

$(singletons [d|
  data MyProxy (a :: Type) = MyProxy

  f MyProxy =
    let x = let z :: MyProxy a -- When singled, this `a` should be explicitly quantified
                z = MyProxy in z
    in x
  |])
