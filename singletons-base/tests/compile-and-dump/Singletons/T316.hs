module T316 where

import Data.Kind
import Data.Singletons.Prelude
import Data.Singletons.TH

$(promoteOnly [d|
    replaceAllGTypes :: (a -> Type -> a) -> [Type] -> [a] -> [a]
    replaceAllGTypes f types as = zipWith f as types
  |])
