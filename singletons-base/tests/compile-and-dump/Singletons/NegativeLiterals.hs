{-# LANGUAGE NegativeLiterals #-}

module Singletons.NegativeLiterals where

import Data.Singletons.Base.TH
import Numeric.Natural (Natural)

$(singletons [d|
  f :: Natural
  f = -1
  |])
