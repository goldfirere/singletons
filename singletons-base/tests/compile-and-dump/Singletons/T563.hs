{-# LANGUAGE NoFieldSelectors #-}
module T563 where

import Data.Singletons.Base.TH
import Prelude.Singletons

$(singletons [d|
  infixr 6 `unFoo`
  data Foo = MkFoo { unFoo :: Bool }
  |])

-- This should not compile:
$(singletonsOnly [d|
  unFoo' :: Foo -> Bool
  unFoo' = unFoo
  |])
