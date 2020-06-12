module Singletons.Maybe where

import Data.Singletons.Prelude.TH

$(singletons [d|
  data Maybe a = Nothing | Just a deriving (Eq, Show)
 |])
