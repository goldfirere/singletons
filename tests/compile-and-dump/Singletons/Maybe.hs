{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Singletons.Maybe where

import Data.Singletons.TH
import Data.Singletons.SuppressUnusedWarnings
import Prelude hiding (Maybe, Nothing, Just)

-- Work around #190
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Show

$(singletons [d|
  data Maybe a = Nothing | Just a deriving (Eq, Show)
 |])
