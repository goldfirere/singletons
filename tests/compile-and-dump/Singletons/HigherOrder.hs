module Singletons.HigherOrder where

import Data.Singletons.TH
import Data.Singletons.List
import Data.Singletons.Maybe
import Data.Proxy

$(singletons [d|
  map :: (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (h:t) = (f h) : (map f t)

  liftMaybe :: (a -> b) -> Maybe a -> Maybe b
  liftMaybe f (Just x) = Just (f x)
  liftMaybe _ Nothing = Nothing
 |])
