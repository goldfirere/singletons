{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Eq
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the SEq singleton version of the Eq type class.
--
-----------------------------------------------------------------------------

module Data.Singletons.Eq (
  SEq(..),
  type (==), (:==), (:/=)
  ) where

import Data.Singletons.Util
import Data.Singletons.Bool
import Data.Singletons
import Data.Singletons.Singletons
import Data.Singletons.Instances
import GHC.TypeLits ( Nat, Symbol )
import Unsafe.Coerce   -- for TypeLits instances
import Data.Singletons.Types
#if __GLASGOW_HASKELL__ < 707
import Data.Singletons.Promote ( promoteEqInstances )
#endif

-- | A type synonym conforming to singletons naming conventions
type a :/= b = Not (a :== b)
               
-- | The singleton analogue of 'Eq'. Unlike the definition for 'Eq', it is required
-- that instances define a body for '(%:==)'. You may also supply a body for '(%:/=)'.
class (kparam ~ 'KProxy) => SEq (kparam :: KProxy k) where
  -- | Boolean equality on singletons
  (%:==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :== b)

  -- | Boolean disequality on singletons
  (%:/=) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (a :/= b)
  a %:/= b = sNot (a %:== b)


#if __GLASGOW_HASKELL__ < 707
$(promoteEqInstances basicTypes)   -- these instances are in Data.Type.Equality
#endif

$(singEqInstancesOnly basicTypes)
