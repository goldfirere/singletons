module Singletons.HigherOrder where

import Data.Singletons.TH
import Data.Singletons.List hiding (map, sMap, Map, MapSym0, MapSym1)
import Data.Singletons.Maybe
import Data.Proxy

$(singletons [d|
  map :: (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (h:t) = (f h) : (map f t)

  liftMaybe :: (a -> b) -> Maybe a -> Maybe b
  liftMaybe f (Just x) = Just (f x)
  liftMaybe _ Nothing = Nothing

  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
  zipWith _ [] []         = []

  -- higher order function that accepts higher order function
  foo :: ((a -> b) -> a -> b) -> (a -> b)  -> a -> b
  foo f g a = f g a
 |])
