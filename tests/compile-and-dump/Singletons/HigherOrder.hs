
module Singletons.HigherOrder where

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.List hiding (
           sMap, Map, MapSym0, MapSym1, ZipWith, sZipWith, ZipWithSym0,
           ZipWithSym1, ZipWithSym2 )
import Data.Singletons.Maybe
import Singletons.Nat
import Prelude hiding (Either(..))

$(singletons [d|
  data Either a b = Left a | Right b

  map :: (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (h:t) = (f h) : (map f t)

  liftMaybe :: (a -> b) -> Maybe a -> Maybe b
  liftMaybe f (Just x) = Just (f x)
  liftMaybe _ Nothing = Nothing

  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
  zipWith _ [] []         = []
  zipWith _ (_:_) []      = []
  zipWith _ [] (_:_)      = []

  foo :: ((a -> b) -> a -> b) -> (a -> b)  -> a -> b
  foo f g a = f g a

  splunge :: [Nat] -> [Bool] -> [Nat]
  splunge ns bs = zipWith (\n b -> if b then Succ (Succ n) else n) ns bs

  etad :: [Nat] -> [Bool] -> [Nat]
  etad = zipWith (\n b -> if b then Succ (Succ n) else n)

 |])

foo1a :: Proxy (ZipWith (TyCon2 Either) '[Int, Bool] '[Char, Double])
foo1a = Proxy

foo1b :: Proxy ('[Either Int Char, Either Bool Double])
foo1b = foo1a

foo2a :: Proxy (Map (TyCon1 (Either Int)) '[Bool, Double])
foo2a = Proxy

foo2b :: Proxy ('[Either Int Bool, Either Int Double])
foo2b = foo2a

foo3a :: Proxy (Map PredSym0 '[Succ Zero, Succ (Succ Zero)])
foo3a = Proxy

foo3b :: Proxy '[Zero, Succ Zero]
foo3b = foo3a
