module T287 where

import Data.Singletons.TH

$(singletons [d|
  class S a where
    (<<>>) :: a -> a -> a

  instance S b => S (a -> b) where
    f <<>> g = \x -> f x <<>> g x
  |])
