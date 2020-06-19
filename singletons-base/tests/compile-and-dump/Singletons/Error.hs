module Singletons.Error where

import Data.Singletons
import Data.Singletons.TH
import Prelude.Singletons hiding (Head, HeadSym0, HeadSym1, sHead)

$(singletons [d|
  head :: [a] -> a
  head (a : _) = a
  head []      = error "head: empty list"
 |])
