module Singletons.Empty where

import Data.Singletons.TH

$(singletons [d|
  data Empty
 |])
