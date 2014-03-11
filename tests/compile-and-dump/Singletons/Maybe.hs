module Singletons.Maybe where

import Data.Singletons.TH
import Data.Singletons.Prelude hiding (NothingSym0, JustSym0, SNothing, SJust)
import Prelude hiding (Maybe, Nothing, Just)

$(singletons [d|
  data Maybe a = Nothing | Just a deriving (Eq, Show)
 |])
