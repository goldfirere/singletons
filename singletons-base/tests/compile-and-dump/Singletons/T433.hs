module T433 where

import Data.Singletons.Base.TH
import Prelude.Singletons

$(promote [d|
  konst :: a -> Bool -> a
  konst x _ = x

  f local = g
    where
      g :: forall a. a -> a
      g x = konst (x :: a) local
  |])
