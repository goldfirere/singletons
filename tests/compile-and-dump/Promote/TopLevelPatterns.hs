module Promote.TopLevelPatterns where

import Data.Singletons.TH
import Data.Singletons.List

$(promote [d|
  data Bool = False | True

  otherwise :: Bool
  otherwise = True

  f,g :: Bool
  [f,g] = [otherwise, otherwise]
 |])
