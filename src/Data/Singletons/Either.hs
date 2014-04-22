{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeFamilies, GADTs,
             DataKinds, PolyKinds, RankNTypes, UndecidableInstances, CPP #-}

#if __GLASGOW_HASKELL__ < 707
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#else
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Either
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'Either',
-- including a singletons version of all the definitions in @Data.Either@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Either@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Either (
  -- * The 'Either' singleton
  Sing(SLeft, SRight),
  -- | Though Haddock doesn't show it, the 'Sing' instance above declares
  -- constructors
  --
  -- > SLeft  :: Sing a -> Sing (Left a)
  -- > SRight :: Sing b -> Sing (Right b)

  SEither,
  -- | 'SEither' is a kind-restricted synonym for 'Sing':
  -- @type SEither (a :: Either x y) = Sing a@

  -- * Singletons from @Data.Either@
  either_, Either_, sEither_,
  -- | The preceding two definitions are derived from the function 'either' in
  -- @Data.Either@. The extra underscore is to avoid name clashes with the type
  -- 'Either'.

  Lefts, sLefts, Rights, sRights,
  PartitionEithers, sPartitionEithers, IsLeft, sIsLeft, IsRight, sIsRight,

  -- * Defunctionalization symbols
  LeftSym0, RightSym0,

  Either_Sym0, Either_Sym1, Either_Sym2,
  LeftsSym0, RightsSym0,
  PartitionEithersSym0, PartitionEithers_auxSym0, PartitionEithers_auxSym1,
  IsLeftSym0, IsRightSym0
  ) where

import Data.Singletons.Instances
import Data.Singletons.TH
import Data.Singletons.List

$(singletons [d|
  -- | Case analysis for the 'Either' type.
  -- If the value is @'Left' a@, apply the first function to @a@;
  -- if it is @'Right' b@, apply the second function to @b@.
  either_                  :: (a -> c) -> (b -> c) -> Either a b -> c
  either_ f _ (Left x)     =  f x
  either_ _ g (Right y)    =  g y
 |])

$(singletonsOnly [d|
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
