module T180 where

import Data.Singletons.TH
import Data.Singletons.Prelude

promote [d|
  data X = X1 {y :: Symbol} | X2 {y :: Symbol}
  z (X1 x) = x
  z (X2 x) = x
  |]
