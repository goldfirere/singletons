module Singletons.Error where

import Data.Singletons.TH
import Data.Singletons.Prelude hiding (Head, HeadSym0)

$(singletons [d|
  head :: [a] -> a
  head (a : _) = a
  head []      = error "Data.Singletons.List.head: empty list"
 |])
