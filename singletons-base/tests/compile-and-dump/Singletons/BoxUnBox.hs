module Singletons.BoxUnBox where

import Data.Singletons.TH

$(singletons [d|
  data Box a = FBox a
  unBox :: Box a -> a
  unBox (FBox a) = a
 |])
