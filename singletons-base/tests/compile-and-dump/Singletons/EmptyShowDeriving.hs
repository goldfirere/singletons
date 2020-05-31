module Singletons.EmptyShowDeriving where

import Data.Singletons.Prelude.TH

$(singletons [d| data Foo
                 deriving instance Show Foo
               |])
