module T565 where

import Data.Singletons.TH

$(singletons [d|
  data C a where
    D :: forall {a}. C a
  |])
