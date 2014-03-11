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
  Refuted, Decision(..),
  TyFun, TyCon, type (@@),
  KProxy(..), Proxy(..),
  (:~:)(..), gcastWith, TestEquality(..),
  Not, If, type (==), (:==)
  ) where

import Data.Singletons.Void

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

type family (a :: k) :== (b :: k) :: Bool
type a == b = a :== b

type family Not (b :: Bool) :: Bool
type instance Not True  = False
type instance Not False = True

#else

import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool

-- | A re-export of the type-level @(==)@ that conforms to the singletons naming
-- convention.
type a :== b = a == b

#endif

-- | Representation of a type-level function. The difference between
-- term-level arrows and this type-level arrow is that at the term
-- level applications can be unsaturated, whereas at the type level
-- all applications have to be fully saturated.
data TyFun :: * -> * -> *

-- | Wrapper for converting term level arrows into type level
-- arrows. TyCon is designed to allow usage of term-level functions
-- where type-level function is expected. For example given:
--
-- > data Nat = Zero | Succ Nat
-- > type family Map (a :: TyFun a b -> *) (a :: [a]) :: [b]
-- >   Map f '[] = '[]
-- >   Map f (x ': xs) = Apply f x ': Map f xs
--
-- We can write:
--
-- > Map (TyCon Succ) [Zero, Succ Zero]
data TyCon :: (k1 -> k2) -> (TyFun k1 k2) -> *

-- | Type level function application
type family (f :: TyFun k1 k2 -> *) @@ (x :: k1) :: k2
type instance (TyCon f) @@ x = f x
infixl 9 @@

-- | Because we can never create a value of type 'Void', a function that type-checks
-- at @a -> Void@ shows that objects of type @a@ can never exist. Thus, we say that
-- @a@ is 'Refuted'
type Refuted a = (a -> Void)

-- | A 'Decision' about a type @a@ is either a proof of existence or a proof that @a@
-- cannot exist.
data Decision a = Proved a               -- ^ Witness for @a@
                | Disproved (Refuted a)  -- ^ Proof that no @a@ exists
