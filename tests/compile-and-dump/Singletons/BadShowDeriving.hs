module Singletons.BadShowDeriving where

import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d| data Foo deriving Show |])
