module T402 where

import Data.Kind
import Data.Singletons.TH

type family Any :: k
$(singletons [d| type AnyOfKind (k :: Type) = Any :: k |])
