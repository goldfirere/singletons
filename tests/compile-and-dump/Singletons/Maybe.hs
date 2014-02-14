module Singletons.Maybe where

import Data.Singletons.TH

$(singletons [d|
  data Maybe a = Nothing | Just a deriving (Eq, Show)
 |])
