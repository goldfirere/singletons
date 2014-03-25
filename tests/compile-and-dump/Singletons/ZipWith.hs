module Singletons.ZipWith where

import Data.Singletons.TH
import Data.Singletons.Prelude hiding (zipWith, ZipWith, sZipWith, ZipWithSym0,
                                       ZipWithSym1, ZipWithSym2 )

$(singletons [d|
  data Nat = Zero | Succ Nat

  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
  zipWith _ []     []     = []
  zipWith _ []     (_:_)  = []
  zipWith _ (_:_)  []     = []

  splunge :: [Nat] -> [Bool] -> [Nat]
  splunge ns bs = zipWith (\n b -> if b then Succ (Succ n) else n) ns bs

  etad :: [Nat] -> [Bool] -> [Nat]
  etad = zipWith (\n b -> if b then Succ (Succ n) else n)
  |])
