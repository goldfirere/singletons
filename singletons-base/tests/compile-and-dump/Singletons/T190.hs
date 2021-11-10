module T190 where

import Data.Singletons.Base.TH

$(singletons [d| data T = T deriving (Eq, Ord, Enum, Bounded, Show) |])
