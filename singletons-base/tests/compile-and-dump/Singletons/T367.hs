module T367 where

import Data.Singletons.TH (singletonsOnly)
import Prelude.Singletons

$(singletonsOnly [d|
  const' :: a -> b -> a
  const' x _ = x
  |])

test :: Sing True
test = sConst' @Bool @() STrue STuple0
