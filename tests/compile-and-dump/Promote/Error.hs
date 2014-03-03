module Promote.Error where

import Data.Singletons.TH
import Data.Singletons.Prelude hiding (head, Head, HeadSym0)

$(promote [d|
  head :: [a] -> a
  head (a : _) = a
  head []      = error "Data.Singletons.List.head: empty list"
 |])
