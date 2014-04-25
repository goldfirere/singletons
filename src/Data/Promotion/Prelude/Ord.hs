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
--  type (==), (:==), (:/=), (:==$), (:==$$), (:==$$$), (:/=$), (:/=$$), (:/=$$$)
  ) where

import Data.Singletons.TH

$(promote [d|
  class  (Eq a) => Ord a  where
      compare              :: a -> a -> Ordering
      (<), (<=), (>), (>=) :: a -> a -> Bool
      max, min             :: a -> a -> a
 |])
{-
    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
-}
