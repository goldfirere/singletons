module Singletons.T78 where

import Data.Singletons.TH
import Data.Singletons.Prelude

type MaybeBool = Maybe Bool

$(singletons [d|
  foo :: MaybeBool -> Bool
  foo (Just False) = False
  foo (Just True)  = True
  foo Nothing      = False
  |])
