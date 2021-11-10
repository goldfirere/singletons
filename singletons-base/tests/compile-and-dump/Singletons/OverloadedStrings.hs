{-# LANGUAGE OverloadedStrings #-}

module OverloadedStrings where

import Data.Singletons.Base.TH
import GHC.TypeLits.Singletons

$(singletons
  [d| symId :: Symbol -> Symbol
      symId x = x

      foo :: Symbol
      foo = symId "foo"
    |])
