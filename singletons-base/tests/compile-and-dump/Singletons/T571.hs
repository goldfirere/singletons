module T571 where

import Data.Singletons.TH

$(singletons [d|
  f :: a -> a
  f x = x
  |])

$(singletons [d|
  g :: (a -> a) -> a -> a
  g f x = f x
  |])
