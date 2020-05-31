module T353 where

import Data.Kind
import Data.Proxy
import Data.Singletons.TH

$(singletons [d|
  type family Symmetry (a :: Proxy t) (y :: Proxy t)
                       (e :: (a :: Proxy (t :: k)) :~: (y :: Proxy (t :: k))) :: Type where
    Symmetry a y _ = y :~: a
  |])

data Prod f g p = MkProd (f p) (g p)
$(genDefunSymbols [''Prod])

data Foo a b = MkFoo (Proxy a) (Proxy b)
$(genDefunSymbols [''Foo])
