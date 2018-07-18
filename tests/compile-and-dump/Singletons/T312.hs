module T312 where

import Data.Singletons.TH

$(singletons [d|
  class Foo a where
    bar :: a -> b -> b
    bar _ x = x

    baz :: forall b. a -> b -> b
    baz = h where
      h :: forall c. c -> b -> b
      h _ x = x
  |])
