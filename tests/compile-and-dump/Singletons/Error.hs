module Singletons.Error where

import Data.Singletons
import Data.Singletons.Prelude hiding (Head, HeadSym0, HeadSym1, sHead)
import Data.Singletons.TH

$(singletons [d|
  head :: [a] -> a
  head (a : _) = a
  head []      = error "Data.Singletons.List.head: empty list"
 |])
