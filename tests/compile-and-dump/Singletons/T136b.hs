module T136b where

import Data.Singletons.TH
import Data.Singletons.Prelude.Bool

$(singletons [d|
  class C a where
    meth :: a -> a
  |])

$(singletons [d|
  instance C Bool where
    meth = not
  |])
