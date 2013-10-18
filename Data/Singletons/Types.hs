{- Data/Singletons/Types.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains definitions of types useful within singletons.
-}

{-# LANGUAGE PolyKinds, TypeOperators, GADTs, RankNTypes, CPP #-}

module Data.Singletons.Types (
  KProxy(..), Proxy(..),
  (:~:)(..), gcastWith, EqualityT(..),
  Void, Refuted, Decision(..)
  ) where

import Data.Void

#if __GLASGOW_HASKELL__ >= 707

import Data.Proxy        
import Data.Type.Equality

#else

-- now in Data.Proxy
data KProxy (a :: *) = KProxy
data Proxy a = Proxy

-- now in Data.Type.Equality
data a :~: b where
  Refl :: a :~: a

class EqualityT (f :: k -> *) where
  equalsT :: f a -> f b -> Maybe (a :~: b)

#endif

gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

type Refuted a = (a -> Void)
data Decision a = Proved a | Disproved (Refuted a)