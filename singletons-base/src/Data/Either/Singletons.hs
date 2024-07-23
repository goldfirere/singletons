{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Singletons
-- Copyright   :  (C) 2013-2014 Richard Eisenberg, Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for 'Either',
-- including singled versions of all the definitions in @Data.Either@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Either@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Either.Singletons (
  -- * The 'Either' singleton
  Sing, SEither(..),

  -- * Singletons from @Data.Either@
  either_, Either_, sEither_,
  -- | The preceding two definitions are derived from the function 'either' in
  -- @Data.Either@. The extra underscore is to avoid name clashes with the type
  -- 'Either'.

  Lefts, sLefts, Rights, sRights,
  PartitionEithers, sPartitionEithers, IsLeft, sIsLeft, IsRight, sIsRight,

  -- * Defunctionalization symbols
  LeftSym0, LeftSym1, RightSym0, RightSym1,

  Either_Sym0, Either_Sym1, Either_Sym2, Either_Sym3,
  LeftsSym0, LeftsSym1, RightsSym0, RightsSym1,
  IsLeftSym0, IsLeftSym1, IsRightSym0, IsRightSym1
  ) where

import Data.Singletons.Base.Instances
import Data.Singletons.TH
import GHC.Base.Singletons

-- NB: The haddock comments are disabled because TH can't deal with them.

$(singletons [d|
  -- Renamed to avoid name clash
  -- -| Case analysis for the 'Either' type.
  -- If the value is @'Left' a@, apply the first function to @a@;
  -- if it is @'Right' b@, apply the second function to @b@.
  either_                  :: (a -> c) -> (b -> c) -> Either a b -> c
  either_ f _ (Left x)     =  f x
  either_ _ g (Right y)    =  g y
 |])

$(singletonsOnly [d|
  -- -| Extracts from a list of 'Either' all the 'Left' elements
  -- All the 'Left' elements are extracted in order.

  -- Modified to avoid list comprehensions
  lefts   :: [Either a b] -> [a]
  lefts []             = []
  lefts (Left x  : xs) = x : lefts xs
  lefts (Right _ : xs) = lefts xs

  -- -| Extracts from a list of 'Either' all the 'Right' elements
  -- All the 'Right' elements are extracted in order.

  -- Modified to avoid list comprehensions
  rights   :: [Either a b] -> [b]
  rights []             = []
  rights (Left _  : xs) = rights xs
  rights (Right x : xs) = x : rights xs

  -- -| Partitions a list of 'Either' into two lists
  -- All the 'Left' elements are extracted, in order, to the first
  -- component of the output.  Similarly the 'Right' elements are extracted
  -- to the second component of the output.
  partitionEithers :: [Either a b] -> ([a],[b])
  partitionEithers = foldr (either_ left right) ([],[])
   where
    left  a (l, r) = (a:l, r)
    right a (l, r) = (l, a:r)

  -- -| Return `True` if the given value is a `Left`-value, `False` otherwise.
  --
  -- /Since: 4.7.0.0/
  isLeft :: Either a b -> Bool
  isLeft (Left  _) = True
  isLeft (Right _) = False

  -- -| Return `True` if the given value is a `Right`-value, `False` otherwise.
  --
  -- /Since: 4.7.0.0/
  isRight :: Either a b -> Bool
  isRight (Left  _) = False
  isRight (Right _) = True
  |])
