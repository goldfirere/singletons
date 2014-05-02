{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs, CPP, DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Eq
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the SEq singleton version of the Eq type class.
--
-----------------------------------------------------------------------------

module Data.Singletons.Prelude.Eq (
  PEq(..), SEq(..),
  (:==$), (:==$$), (:==$$$), (:/=$), (:/=$$), (:/=$$$)
  ) where

import Data.Singletons.Prelude.Bool
import Data.Singletons
import Data.Singletons.Single
import Data.Singletons.Prelude.Instances
import Data.Singletons.Util
import Data.Singletons.Promote

#if __GLASGOW_HASKELL__ >= 707
import Data.Type.Equality
#endif

-- | The promoted analogue of 'Eq'. If you supply no definition for '(:==)' under
-- GHC 7.8+, then it defaults to a use of '(==)', from @Data.Type.Equality@.
class kproxy ~ 'KProxy => PEq (kproxy :: KProxy a) where
  type (:==) (x :: a) (y :: a) :: Bool
  type (:/=) (x :: a) (y :: a) :: Bool

#if __GLASGOW_HASKELL__ < 707
  type (x :: a) :== (y :: a) = Not (x :/= y)
#else
  type (x :: a) :== (y :: a) = x == y
#endif
  type (x :: a) :/= (y :: a) = Not (x :== y)

$(genDefunSymbols [''(:==), ''(:/=)])

-- | The singleton analogue of 'Eq'. Unlike the definition for 'Eq', it is required
-- that instances define a body for '(%:==)'. You may also supply a body for '(%:/=)'.
class (kparam ~ 'KProxy) => SEq (kparam :: KProxy k) where
  -- | Boolean equality on singletons
  (%:==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :== b)

  -- | Boolean disequality on singletons
  (%:/=) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :/= b)
  default (%:/=) :: forall (a :: k) (b :: k).
                    ((a :/= b) ~ Not (a :== b))
                 => Sing a -> Sing b -> Sing (a :/= b)
  a %:/= b = sNot (a %:== b)

$(singEqInstances basicTypes)
