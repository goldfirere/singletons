module T136b where

import Data.Bool.Singletons
import Data.Singletons.TH

$(singletons [d|
  class C a where
    meth :: a -> a
  |])

$(singletons [d|
  instance C Bool where
    meth = not
  |])
