{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Singletons.T54 where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  g :: Bool -> Bool
  g e = (case [not] of
            [_] -> not) e
  |])
