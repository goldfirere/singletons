{-# LANGUAGE NegativeLiterals #-}
module Singletons.NegativeLiterals where

import Data.Singletons.TH
import GHC.TypeNats (Nat)

$(singletons [d|
  f :: Nat
  f = -1
  |])
