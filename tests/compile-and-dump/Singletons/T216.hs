module T216 where

import Data.Kind
import Data.Singletons.TH

type family MyProxy k (a :: k) :: Type where
  MyProxy _ a = Proxy a

type family Symmetry (a :: t) (y :: t) (e :: a :~: y) :: Type where
  Symmetry a y _ = y :~: a

$(genDefunSymbols [''MyProxy, ''Symmetry])
