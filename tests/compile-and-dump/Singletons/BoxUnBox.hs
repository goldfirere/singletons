{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Singletons.BoxUnBox where

import Data.Singletons.TH
import Data.Singletons.SuppressUnusedWarnings

$(singletons [d|
  data Box a = FBox a
  unBox :: Box a -> a
  unBox (FBox a) = a
 |])
