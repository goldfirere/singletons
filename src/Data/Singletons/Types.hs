{-# LANGUAGE PolyKinds, TypeOperators, GADTs, RankNTypes, TypeFamilies,
             CPP, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines and exports types that are useful when working with singletons.
-- Some of these are re-exports from @Data.Type.Equality@.
--
----------------------------------------------------------------------------


module Data.Singletons.Types (
  KProxy(..), Proxy(..),
  (:~:)(..), gcastWith, TestEquality(..),
  Not, If
  ) where

#if __GLASGOW_HASKELL__ < 707

-- now in Data.Proxy
data KProxy (a :: *) = KProxy
data Proxy a = Proxy

-- now in Data.Type.Equality
data a :~: b where
  Refl :: a :~: a

gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

class TestEquality (f :: k -> *) where
  testEquality :: f a -> f b -> Maybe (a :~: b)

-- now in Data.Type.Bool
-- | Type-level "If". @If True a b@ ==> @a@; @If False a b@ ==> @b@
type family If (a :: Bool) (b :: k) (c :: k) :: k
type instance If 'True b c = b
type instance If 'False b c = c

type family Not (b :: Bool) :: Bool
type instance Not True  = False
type instance Not False = True

#else

import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool

#endif

