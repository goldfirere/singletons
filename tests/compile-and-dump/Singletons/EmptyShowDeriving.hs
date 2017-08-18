module Singletons.EmptyShowDeriving where

import Data.Singletons.TH

$(singletons [d| data Foo
                 deriving instance Show Foo
               |])
