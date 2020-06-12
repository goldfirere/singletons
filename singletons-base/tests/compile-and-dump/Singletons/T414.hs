module T414 where

import Data.Kind
import Data.Singletons.TH

$(singletons [d|
  class C1 (a :: Bool) where
    type T1 a b

  class C2 a where
    type T2 a b

  type C3 :: Bool -> Constraint
  class C3 a where
    type T3 a b
  |])
