{-# LANGUAGE OverloadedStrings #-}
module OverloadedStrings where

import Data.Singletons.TH
import Data.Singletons.TypeLits

$(singletons
  [d| symId :: Symbol -> Symbol
      symId x = x

      foo :: Symbol
      foo = symId "foo"
    |])
