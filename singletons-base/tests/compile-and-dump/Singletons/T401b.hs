module T401b where

import Data.Singletons.TH

$(singletons [d|
  newtype T where
    MkT :: (forall a. a) -> T
  |])
