module Singletons.EnumDeriving where

import Data.Singletons.Base.TH

$(singletons [d|
  data Foo = Bar | Baz | Bum
    deriving Enum
  data Quux = Q1 | Q2
  |])

$(singEnumInstance ''Quux)
