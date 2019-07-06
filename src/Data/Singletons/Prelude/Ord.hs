{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables,
             TypeFamilies, TypeOperators, GADTs, UndecidableInstances,
             FlexibleContexts, DefaultSignatures, InstanceSigs,
             StandaloneDeriving, FlexibleInstances, TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Ord
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of Ord, 'POrd', and the singleton version,
-- 'SOrd'.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Ord (
  POrd(..), SOrd(..),

  Comparing, sComparing,

  -- | 'thenCmp' returns its second argument if its first is 'EQ'; otherwise,
  -- it returns its first argument.
  thenCmp, ThenCmp, sThenCmp,

  Sing, SOrdering(..), SDown(..),

  -- ** Defunctionalization symbols
  ThenCmpSym0, ThenCmpSym1, ThenCmpSym2,
  LTSym0, EQSym0, GTSym0,
  CompareSym0, CompareSym1, CompareSym2,
  type (<@#@$),  type (<@#@$$),  type (<@#@$$$),
  type (<=@#@$), type (<=@#@$$), type (<=@#@$$$),
  type (>@#@$),  type (>@#@$$),  type (>@#@$$$),
  type (>=@#@$), type (>=@#@$$), type (>=@#@$$$),
  MaxSym0, MaxSym1, MaxSym2,
  MinSym0, MinSym1, MinSym2,
  ComparingSym0, ComparingSym1, ComparingSym2, ComparingSym3,
  DownSym0, DownSym1
  ) where

import Data.Ord (Down(..))
import Data.Singletons.Single
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Util

$(singletonsOnly [d|
  class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    infix 4 <=
    infix 4 <
    infix 4 >
    infix 4 >=
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  EQ -> False; GT -> False }
    x <= y = case compare x y of { LT -> True;  EQ -> True;  GT -> False }
    x >  y = case compare x y of { LT -> False; EQ -> False; GT -> True }
    x >= y = case compare x y of { LT -> False; EQ -> True;  GT -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
    -- Not handled by TH: {-# MINIMAL compare | (<=) #-}

  -- -|
  -- > comparing p x y = compare (p x) (p y)
  --
  -- Useful combinator for use in conjunction with the @xxxBy@ family
  -- of functions from "Data.List", for example:
  --
  -- >   ... sortBy (comparing fst) ...
  comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
  comparing p x y = compare (p x) (p y)
  |])

-- Workaround for #326
infix 4 <=
infix 4 <
infix 4 >
infix 4 >=

$(genSingletons [''Down])

$(singletonsOnly [d|
  deriving instance Eq a => Eq (Down a)

  instance Ord a => Ord (Down a) where
      compare (Down x) (Down y) = y `compare` x
  |])

$(singletons [d|
  thenCmp :: Ordering -> Ordering -> Ordering
  thenCmp EQ x = x
  thenCmp LT _ = LT
  thenCmp GT _ = GT
  |])

$(singOrdInstances basicTypes)
