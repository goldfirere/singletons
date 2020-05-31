module T332 where

import Data.Singletons.Prelude.TH

$(promote [d|
  data Foo = MkFoo

  f :: Foo -> ()
  f MkFoo{} = ()
  |])

$(singletons [d|
  data Bar = MkBar

  b :: Bar -> ()
  b MkBar{} = ()
  |])
