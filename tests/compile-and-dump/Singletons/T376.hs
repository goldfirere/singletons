module T376 where

import Data.Singletons.TH

$(singletons [d|
  f :: (() -> ()) -> (() -> ())
  f g = g :: () -> ()
  |])
