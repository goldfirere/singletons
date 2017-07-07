{-# OPTIONS_GHC -Wno-unused-imports #-}

module Singletons.Maybe where

import Data.Singletons.TH

-- Work around #190
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Show

$(singletons [d|
  data Maybe a = Nothing | Just a deriving (Eq, Show)
 |])
