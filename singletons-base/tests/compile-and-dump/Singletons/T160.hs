module T160 where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeError

$(singletons
  [d| foo :: (Num a, Eq a) => a -> a
      foo x = if x == 0 then 1 else typeError $ ShowType x
    |])

f :: Foo 1 :~: 42
f = Refl
