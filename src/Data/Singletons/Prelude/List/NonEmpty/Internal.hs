-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.List.NonEmpty.Internal
-- Copyright   :  (C) 2016 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Renames a bunch of List functions because singletons can't support qualified
-- names. :(
--
----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeInType, TypeFamilies,
             UndecidableInstances, GADTs #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.Singletons.Prelude.List.NonEmpty.Internal where

import Data.Singletons.Single
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Eq
import Data.List
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
  |])

$(singletonsOnly [d|
  listtake :: Nat -> [a] -> [a]
  listtake = take

  listdrop :: Nat -> [a] -> [a]
  listdrop = drop

  listsplitAt :: Nat -> [a] -> ([a], [a])
  listsplitAt = splitAt

  listindex :: [a] -> Nat -> a
  listindex = (!!)

  listlength :: [a] -> Nat
  listlength = length
  |])

listtake :: Nat -> [a] -> [a]
listtake = undefined

listdrop :: Nat -> [a] -> [a]
listdrop = undefined

listsplitAt :: Nat -> [a] -> ([a], [a])
listsplitAt = undefined

listindex :: [a] -> Nat -> a
listindex = undefined

listlength :: [a] -> Nat
listlength = undefined
