{-# OPTIONS_GHC -Wall #-}
module Singletons.Undef where

import Data.Singletons.TH

$(singletons [d|
  foo :: Bool -> Bool
  foo = undefined

  bar :: Bool -> Bool
  bar = error "urk"
  |])
