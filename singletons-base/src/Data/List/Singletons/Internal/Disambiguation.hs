{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Singletons.Internal.Disambiguation
-- Copyright   :  (C) 2016 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Renames a bunch of List functions because singletons can't support qualified
-- names. :(
--
----------------------------------------------------------------------------

module Data.List.Singletons.Internal.Disambiguation where

import Data.Eq.Singletons
import Data.List ( inits, insert, intersperse, isPrefixOf
                 , nubBy, partition, sort, sortBy, tails, transpose )
import Data.List.Singletons.Internal
import Data.Ord.Singletons
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import GHC.Base.Singletons
import GHC.Num.Singletons
import GHC.TypeLits

-- singletons doesn't support qualified names :(
$(singletons [d|
  listlast :: [a] -> a
  listlast = last

  listinit :: [a] -> [a]
  listinit = init

  listsort :: Ord a => [a] -> [a]
  listsort = sort

  listinits :: [a] -> [[a]]
  listinits = inits

  listtails :: [a] -> [[a]]
  listtails = tails

  listinsert :: Ord a => a -> [a] -> [a]
  listinsert = insert

  listscanl :: (b -> a -> b) -> b -> [a] -> [b]
  listscanl = scanl

  listscanr :: (a -> b -> b) -> b -> [a] -> [b]
  listscanr = scanr

  listscanr1 :: (a -> a -> a) -> [a] -> [a]
  listscanr1 = scanr1

  listintersperse :: a -> [a] -> [a]
  listintersperse = intersperse

  listreverse :: [a] -> [a]
  listreverse = reverse

  listtakeWhile :: (a -> Bool) -> [a] -> [a]
  listtakeWhile = takeWhile

  listdropWhile :: (a -> Bool) -> [a] -> [a]
  listdropWhile = dropWhile

  listspan :: (a -> Bool) -> [a] -> ([a], [a])
  listspan = span

  listfilter :: (a -> Bool) -> [a] -> [a]
  listfilter = filter

  listpartition :: (a -> Bool) -> [a] -> ([a], [a])
  listpartition = partition

  listsortBy :: (a -> a -> Ordering) -> [a] -> [a]
  listsortBy = sortBy

  listisPrefixOf :: Eq a => [a] -> [a] -> Bool
  listisPrefixOf = isPrefixOf

  listzip :: [a] -> [b] -> [(a, b)]
  listzip = zip

  listzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  listzipWith = zipWith

  listnubBy :: (a -> a -> Bool) -> [a] -> [a]
  listnubBy = nubBy

  listtranspose :: [[a]] -> [[a]]
  listtranspose = transpose

  listunzip :: [(a,b)] -> ([a],[b])
  listunzip = unzip

  listmap :: (a -> b) -> [a] -> [b]
  listmap = map

  listelem :: Eq a => a -> [a] -> Bool
  listelem = elem

  listfoldl :: (b -> a -> b) -> b -> [a] -> b
  listfoldl = foldl

  listfoldl' :: (b -> a -> b) -> b -> [a] -> b
  listfoldl' = foldl'

  listfoldl1 :: (a -> a -> a) -> [a] -> a
  listfoldl1 = foldl1

  listfoldr :: (a -> b -> b) -> b -> [a] -> b
  listfoldr = foldr

  listfoldr1 :: (a -> a -> a) -> [a] -> a
  listfoldr1 = foldr1

  listmaximum :: Ord a => [a] -> a
  listmaximum = maximum

  listminimum :: Ord a => [a] -> a
  listminimum = minimum

  listnull :: [a] -> Bool
  listnull = null

  listproduct :: Num a => [a] -> a
  listproduct = product

  listsum :: Num a => [a] -> a
  listsum = sum
  |])

$(singletonsOnly [d|
  listtake :: Natural -> [a] -> [a]
  listtake = take

  listdrop :: Natural -> [a] -> [a]
  listdrop = drop

  listsplitAt :: Natural -> [a] -> ([a], [a])
  listsplitAt = splitAt

  listindex :: [a] -> Natural -> a
  listindex = (!!)

  listlength :: [a] -> Nat
  listlength = length
  |])

listtake :: Natural -> [a] -> [a]
listtake = undefined

listdrop :: Natural -> [a] -> [a]
listdrop = undefined

listsplitAt :: Natural -> [a] -> ([a], [a])
listsplitAt = undefined

listindex :: [a] -> Natural -> a
listindex = undefined

listlength :: [a] -> Nat
listlength = undefined
