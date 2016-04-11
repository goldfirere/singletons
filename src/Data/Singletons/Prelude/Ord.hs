{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables,
             TypeFamilies, TypeOperators, GADTs, UndecidableInstances,
             FlexibleContexts, DefaultSignatures, InstanceSigs, CPP #-}
#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE TypeInType #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Ord
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted version of Ord, 'POrd', and the singleton version,
-- 'SOrd'.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Ord (
  POrd(..), SOrd(..),

  -- | 'thenCmp' returns its second argument if its first is 'EQ'; otherwise,
  -- it returns its first argument.
  thenCmp, ThenCmp, sThenCmp,

  Sing(SLT, SEQ, SGT),

  -- ** Defunctionalization symbols
  ThenCmpSym0, ThenCmpSym1, ThenCmpSym2,
  LTSym0, EQSym0, GTSym0,
  CompareSym0, CompareSym1, CompareSym2,
  (:<$), (:<$$), (:<$$$),
  (:<=$), (:<=$$), (:<=$$$),
  (:>$), (:>$$), (:>$$$),
  (:>=$), (:>=$$), (:>=$$$),
  MaxSym0, MaxSym1, MaxSym2,
  MinSym0, MinSym1, MinSym2
  ) where

import Data.Singletons.Single
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Util

#if __GLASGOW_HASKELL__ < 711
import Data.Singletons ( Sing )
#endif

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

  |])

$(singletons [d|
  thenCmp :: Ordering -> Ordering -> Ordering
  thenCmp EQ x = x
  thenCmp LT _ = LT
  thenCmp GT _ = GT
  |])

$(singOrdInstances basicTypes)
