module Singletons.BadShowDeriving where

import Data.Singletons.TH

$(singletons [d| data Foo deriving Show |])
