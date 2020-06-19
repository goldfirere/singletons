module Singletons.EmptyShowDeriving where

import Data.Singletons.Base.TH

$(singletons [d| data Foo
                 deriving instance Show Foo
               |])
