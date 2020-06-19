{-# LANGUAGE ParallelListComp #-}
module T184 where

import Control.Monad
import Control.Monad.Singletons
import Control.Monad.Zip.Singletons
import Data.Singletons.TH
import Prelude.Singletons

$(singletons [d|
  boogie :: Maybe a -> Maybe Bool -> Maybe a
  boogie ma mb = do
    a <- ma
    b <- mb
    guard b
    return a

  zip' :: [a] -> [b] -> [(a, b)]
  zip' xs ys = [(x, y) | x <- xs | y <- ys]

  cartProd :: [a] -> [b] -> [(a, b)]
  cartProd xs ys = [(x, y) | x <- xs, y <- ys]

  trues :: [Bool] -> [Bool]
  trues xs = [x | x <- xs, x]
  |])
