-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Ord
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted version of the Ord type class.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs #-}
module Data.Promotion.Prelude.Ord (
  Compare, CompareSym0, CompareSym1, CompareSym2,
  (:<) , (:<$) , (:<$$) , (:<$$$),
  (:<=), (:<=$), (:<=$$), (:<=$$$),
  (:>) , (:>$) , (:>$$) , (:>$$$),
  (:>=), (:>=$), (:>=$$), (:>=$$$),
  Max, MaxSym0, MaxSym1, MaxSym2,
  Min, MinSym0, MinSym1, MinSym2,
  EQSym0, LTSym0, GTSym0
  ) where

import Data.Singletons.TH
import Data.Singletons.Prelude.Instances (EQSym0, LTSym0, GTSym0)
import Data.Singletons.Util

$(promoteOnly [d|
  class (Eq a) => Ord a where
      compare              :: a -> a -> Ordering
      (<), (<=), (>), (>=) :: a -> a -> Bool
      max, min             :: a -> a -> a
 |])

$(promoteOrdInstances basicTypes)

type family LTDefault (a :: Ordering) where
    LTDefault LTSym0 = TrueSym0
    LTDefault o      = FalseSym0
type instance (:<) (a :: k) (b :: k) = LTDefault (Compare a b)

type family LEDefault (a :: Ordering) where
    LEDefault GTSym0 = FalseSym0
    LEDefault o      = TrueSym0
type instance (:<=) (a :: k) (b :: k) = LEDefault (Compare a b)

type family GTDefault (a :: Ordering) where
    GTDefault GTSym0 = TrueSym0
    GTDefault o      = FalseSym0
type instance (:>) (a :: k) (b :: k) = GTDefault (Compare a b)

type family GEDefault (a :: Ordering) where
    GEDefault LTSym0 = FalseSym0
    GEDefault o      = TrueSym0
type instance (:>=) (a :: k) (b :: k) = GEDefault (Compare a b)

type family MaxDefault (o :: Ordering) (a :: k) (b :: k) where
    MaxDefault LTSym0 a b = b
    MaxDefault o      a b = a
type instance Max (a :: k) (b :: k) = MaxDefault (Compare a b) a b

type family MinDefault (o :: Ordering) (a :: k) (b :: k) where
    MinDefault GTSym0 a b = b
    MinDefault o      a b = a
type instance Min (a :: k) (b :: k) = MinDefault (Compare a b) a b
