module T361 where

import Data.Proxy
import Data.Singletons.TH

$(genDefunSymbols [''Proxy])

$(promote [d|
  f :: Proxy 1 -> Proxy 2
  f Proxy = Proxy
  |])
