module Singletons.BadEnumDeriving where

import Data.Singletons.TH

$(singletons [d|
  data Foo a = Foo a
               deriving Enum
  |])
