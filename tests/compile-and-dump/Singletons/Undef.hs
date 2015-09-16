module Singletons.Undef where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  foo :: Bool -> Bool
  foo = undefined

  bar :: Bool -> Bool
  bar = error "urk"
  |])
