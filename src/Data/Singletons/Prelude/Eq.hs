{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             RankNTypes, FlexibleContexts, TemplateHaskell,
             UndecidableInstances, GADTs, CPP #-}

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
  SEq(..),
  type (==), (:==), (:/=), (:==$), (:==$$), (:==$$$), (:/=$), (:/=$$), (:/=$$$)
  ) where

import Data.Singletons.Prelude.Bool
import Data.Singletons
import Data.Singletons.Single
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.Prelude.Instances
import Data.Singletons.Types
import Data.Singletons.Util
#if __GLASGOW_HASKELL__ < 707
import Data.Singletons.Promote ( promoteEqInstances )
#endif

-- | A type synonym conforming to singletons naming conventions
type a :/= b = Not (a :== b)

type (:==$$$) a b = a :== b

data (:==$$) (a :: k1) (b :: TyFun k1 Bool) where
     (:==$$###) :: Apply ((:==$$) a) arg ~ (:==$$$) a arg
                => Proxy arg
                -> (:==$$) a b
type instance Apply ((:==$$) a) b = (:==$$$) a b

instance SuppressUnusedWarnings (:==$$) where
    suppressUnusedWarnings _ = snd ((:==$$###),())

data (:==$) (a :: TyFun k1 (TyFun k1 Bool -> *)) where
     (:==$###) :: Apply (:==$) arg ~ (:==$$) arg
               => Proxy arg
               -> (:==$) a
type instance Apply (:==$) a = (:==$$) a

instance SuppressUnusedWarnings (:==$) where
    suppressUnusedWarnings _ = snd ((:==$###),())

type (:/=$$$) a b = a :/= b

data (:/=$$) (a :: k1) (b :: TyFun k1 Bool) where
     (:/=$$###) :: Apply ((:/=$$) a) arg ~ (:/=$$$) a arg
                => Proxy arg
                -> (:/=$$) a b
type instance Apply ((:/=$$) a) b = (:/=$$$) a b

instance SuppressUnusedWarnings (:/=$$) where
    suppressUnusedWarnings _ = snd ((:/=$$###),())

data (:/=$) (a :: TyFun k1 (TyFun k1 Bool -> *)) where
     (:/=$###) :: Apply (:/=$) arg ~ (:/=$$) arg
               => Proxy arg
               -> (:/=$) a
type instance Apply (:/=$) a = (:/=$$) a

instance SuppressUnusedWarnings (:/=$) where
    suppressUnusedWarnings _ = snd ((:/=$###),())

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
