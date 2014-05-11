module Promote.BadBoundedDeriving where

import Data.Promotion.Prelude
import Data.Promotion.TH

$(promote [d|
  data Foo a = Foo | Bar a deriving (Bounded)
  |])
