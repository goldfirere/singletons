module T414 where

import Data.Singletons.TH

$(singletons [d|
  class C1 (a :: Bool) where
    type T1 a b

  class C2 a where
    type T2 a b
  |])
