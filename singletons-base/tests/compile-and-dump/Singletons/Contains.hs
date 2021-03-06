module Singletons.Contains where

import Data.Singletons.TH
import Prelude.Singletons

-- polymorphic function with context

$(singletons [d|
  contains :: Eq a => a -> [a] -> Bool
  contains _ [] = False
  contains elt (h:t) = (elt == h) || (contains elt t)
 |])
