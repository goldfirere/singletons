{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             UndecidableInstances, FlexibleContexts, RankNTypes #-}

module Test.Scratch where

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  contains :: Eq a => a -> [a] -> Bool
  contains _ [] = False
  contains elt (h:t) = (elt == h) || (contains elt t)
  |])
