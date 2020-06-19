module T160 where

import Data.Singletons.Base.TypeError
import Data.Singletons.TH
import Prelude.Singletons

$(singletons
  [d| foo :: (Num a, Eq a) => a -> a
      foo x = if x == 0 then 1 else typeError $ ShowType x
    |])

f :: Foo 1 :~: 42
f = Refl
