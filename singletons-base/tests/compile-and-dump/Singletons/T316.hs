module T316 where

import Data.Kind
import Data.Singletons.TH
import Prelude.Singletons

$(promoteOnly [d|
    replaceAllGTypes :: (a -> Type -> a) -> [Type] -> [a] -> [a]
    replaceAllGTypes f types as = zipWith f as types
  |])
