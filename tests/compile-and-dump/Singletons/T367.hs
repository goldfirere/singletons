module T367 where

import Data.Singletons.Prelude
import Data.Singletons.TH (singletonsOnly)

$(singletonsOnly [d|
  const' :: a -> b -> a
  const' x _ = x
  |])

test :: Sing True
test = sConst' @Bool @() STrue STuple0
