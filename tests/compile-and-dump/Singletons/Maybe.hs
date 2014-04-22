module Singletons.Maybe where

import Data.Singletons.TH
import Prelude hiding (Maybe, Nothing, Just)

$(singletons [d|
  data Maybe a = Nothing | Just a deriving (Eq, Show)
 |])
