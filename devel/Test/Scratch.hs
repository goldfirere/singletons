{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             UndecidableInstances, FlexibleContexts, RankNTypes #-}

module Test.Scratch where

import Data.Singletons

$(singletons [d|
  contains :: Eq a => a -> [a] -> Bool
  contains _ [] = False
  contains elt (h:t) = (elt == h) || (contains elt t)
  |])