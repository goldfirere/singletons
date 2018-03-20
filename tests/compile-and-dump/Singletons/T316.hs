module T316 where

import Data.Kind
import Data.Promotion.Prelude
import Data.Promotion.TH

$(promoteOnly [d|
    replaceAllGTypes :: (a -> Type -> a) -> [Type] -> [a] -> [a]
    replaceAllGTypes f types as = zipWith f as types
  |])
