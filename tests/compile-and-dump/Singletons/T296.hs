module T296 where

import Data.Kind
import Data.Singletons.TH

$(singletons [d|
  data MyProxy (a :: Type) = MyProxy

  f :: forall a. MyProxy a -> MyProxy a
  f MyProxy =
    let x = let z :: MyProxy a
                z = MyProxy in z
    in x
  |])
