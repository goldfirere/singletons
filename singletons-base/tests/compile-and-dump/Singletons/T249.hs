module T249 where

import Data.Kind
import Data.Singletons.TH

$(singletons
  [d| data Foo1 a = MkFoo1 a
      data Foo2 a where
        MkFoo2 :: x -> Foo2 x
      data Foo3 a where
        MkFoo3 :: forall x. x -> Foo3 x
    |])
