{- Data/Singletons/Either.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Defines functions and datatypes relating to the singleton for Either.
-}

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeFamilies, GADTs,
             DataKinds, PolyKinds, RankNTypes, UndecidableInstances #-}

module Data.Singletons.Either (
  SEither, Sing(SLeft, SRight),
  Either_, sEither_, Lefts, sLefts, Rights, sRights,
  PartitionEithers, sPartitionEithers, IsLeft, sIsLeft, IsRight, sIsRight
  ) where

import Data.Singletons.Core
import Data.Singletons.TH
import Data.Singletons.List

$(singletonsOnly [d|
  -- | Case analysis for the 'Either' type.
  -- If the value is @'Left' a@, apply the first function to @a@;
  -- if it is @'Right' b@, apply the second function to @b@.
  either_                  :: (a -> c) -> (b -> c) -> Either a b -> c
  either_ f _ (Left x)     =  f x
  either_ _ g (Right y)    =  g y

  -- | Extracts from a list of 'Either' all the 'Left' elements
  -- All the 'Left' elements are extracted in order.

  lefts   :: [Either a b] -> [a]
  lefts []             = []
  lefts (Left x  : xs) = x : lefts xs
  lefts (Right _ : xs) = lefts xs

  -- | Extracts from a list of 'Either' all the 'Right' elements
  -- All the 'Right' elements are extracted in order.

  rights   :: [Either a b] -> [b]
  rights []             = []
  rights (Left _  : xs) = rights xs
  rights (Right x : xs) = x : rights xs

  -- | Partitions a list of 'Either' into two lists
  -- All the 'Left' elements are extracted, in order, to the first
  -- component of the output.  Similarly the 'Right' elements are extracted
  -- to the second component of the output.

  partitionEithers :: [Either a b] -> ([a],[b])
  partitionEithers es = partitionEithers_aux ([], []) es

  partitionEithers_aux :: ([a],[b]) -> [Either a b] -> ([a],[b])
  partitionEithers_aux (as,bs) [] = (reverse as,reverse bs)
  partitionEithers_aux (as,bs) (Left a : es) =
    partitionEithers_aux (a : as, bs) es
  partitionEithers_aux (as,bs) (Right b : es) =
    partitionEithers_aux (as, b : bs) es

  -- | Return `True` if the given value is a `Left`-value, `False` otherwise.
  --
  -- /Since: 4.7.0.0/
  isLeft :: Either a b -> Bool
  isLeft (Left  _) = True
  isLeft (Right _) = False

  -- | Return `True` if the given value is a `Right`-value, `False` otherwise.
  --
  -- /Since: 4.7.0.0/
  isRight :: Either a b -> Bool
  isRight (Left  _) = False
  isRight (Right _) = True
  |])