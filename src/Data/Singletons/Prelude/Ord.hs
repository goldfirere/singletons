{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, ScopedTypeVariables,
             TypeFamilies, TypeOperators, GADTs, UndecidableInstances,
             FlexibleContexts, DefaultSignatures #-}

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

  Sing(SLT, SEQ, SGT),

  LTSym0, EQSym0, GTSym0,
  
  CompareSym0, CompareSym1, CompareSym2,
  (:<$), (:<$$), (:<$$$),
  (:<=$), (:<=$$), (:<=$$$),
  (:>$), (:>$$), (:>$$$),
  (:>=$), (:>=$$), (:>=$$$),
  MaxSym0, MaxSym1, MaxSym2,
  MinSym0, MinSym1, MinSym2
  ) where

import Data.Singletons.Promote
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Bool
import Data.Singletons

$(promoteOnly [d|
  class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

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
    {-# MINIMAL compare | (<=) #-}
  |])

type family CaseOrdering (ord :: Ordering) (lt :: k) (eq :: k) (gt :: k) :: k
type instance CaseOrdering LT lt eq gt = lt
type instance CaseOrdering EQ lt eq gt = eq
type instance CaseOrdering GT lt eq gt = gt

class (kproxy ~ 'KProxy, SEq ('KProxy :: KProxy a))
      => SOrd (kproxy :: KProxy a) where
  sCompare :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (Compare x y)
  (%:<)    :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :< y)
  (%:<=)   :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :<= y)
  (%:>)    :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :> y)
  (%:>=)   :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (x :>= y)
  sMax      :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (Max x y)
  sMin      :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (Min x y)

  default sCompare :: forall (x :: a) (y :: a).
                      (Compare x y ~ If (x :== y) EQ (If (x :<= y) LT GT))
                   => Sing x -> Sing y -> Sing (Compare x y)
  sCompare x y = sIf (x %:== y) SEQ
                     (sIf (x %:<= y) SLT SGT)

  default (%:<) :: forall (x :: a) (y :: a).
                   ((x :< y) ~ CaseOrdering (Compare x y) True False False)
                => Sing x -> Sing y -> Sing (x :< y)
  x %:< y = case sCompare x y of { SLT -> STrue; SEQ -> SFalse; SGT -> SFalse }

  default (%:<=) :: forall (x :: a) (y :: a).
                    ((x :<= y) ~ CaseOrdering (Compare x y) True True False)
                 => Sing x -> Sing y -> Sing (x :<= y)
  x %:<= y = case sCompare x y of { SLT -> STrue; SEQ -> STrue; SGT -> SFalse }

  default (%:>) :: forall (x :: a) (y :: a).
                   ((x :> y) ~ CaseOrdering (Compare x y) False False True)
                => Sing x -> Sing y -> Sing (x :> y)
  x %:> y = case sCompare x y of { SLT -> SFalse; SEQ -> SFalse; SGT -> STrue }

  default (%:>=) :: forall (x :: a) (y :: a).
                    ((x :>= y) ~ CaseOrdering (Compare x y) False True True)
                 => Sing x -> Sing y -> Sing (x :>= y)
  x %:>= y = case sCompare x y of { SLT -> SFalse; SEQ -> STrue; SGT -> STrue }

  default sMax :: forall (x :: a) (y :: a).
                  (Max x y ~ If (x :<= y) y x)
               => Sing x -> Sing y -> Sing (Max x y)
  sMax x y = sIf (x %:<= y) y x

  default sMin :: forall (x :: a) (y :: a).
                  (Min x y ~ If (x :<= y) x y)
               => Sing x -> Sing y -> Sing (Min x y)
  sMin x y = sIf (x %:<= y) x y

-- $(promoteOrdInstances basicTypes)
