{-# LANGUAGE MagicHash, RankNTypes, PolyKinds, GADTs, DataKinds,
             FlexibleContexts, CPP, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the basic definitions to use singletons. For routine
-- use, consider importing 'Data.Singletons.Prelude', which exports constructors
-- for singletons based on types in the @Prelude@.
--
-- You may also want to read
-- <http://www.cis.upenn.edu/~eir/packages/singletons/README.html> and the
-- original paper presenting this library, available at
-- <http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf>.
--
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 707
  -- optimizing instances of SDecide cause GHC to die (#8467)
{-# OPTIONS_GHC -O0 #-}
#endif

module Data.Singletons (
  -- * Main singleton definitions

  Sing,
  -- | See also 'Data.Singletons.Prelude.Sing' for exported constructors

  SingI(..), SingKind(..),

  -- * Working with singletons
  KindOf, Demote,
  SingInstance(..), SomeSing(..),
  singInstance, withSingI, withSomeSing, singByProxy,

#if __GLASGOW_HASKELL__ >= 707
  singByProxy#,
#endif
  withSing, singThat,

  -- * Auxiliary functions
  bugInGHC,
  KProxy(..), Proxy(..)
  ) where

import Unsafe.Coerce
import GHC.TypeLits (Symbol)

#if __GLASGOW_HASKELL__ >= 707
import GHC.Exts ( Proxy# )
import Data.Proxy
#else
import Data.Singletons.Types
#endif

-- | Convenient synonym to refer to the kind of a type variable:
-- @type KindOf (a :: k) = ('KProxy :: KProxy k)@
type KindOf (a :: k) = ('KProxy :: KProxy k)

----------------------------------------------------------------------
---- Sing & friends --------------------------------------------------
----------------------------------------------------------------------
                        
-- | The singleton kind-indexed data family.
data family Sing (a :: k)

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI'.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

-- | The 'SingKind' class is essentially a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  -- | Get a base type from a proxy for the promoted kind. For example,
  -- @DemoteRep ('KProxy :: KProxy Bool)@ will be the type @Bool@.
  type DemoteRep kparam :: *

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> DemoteRep kparam

  -- | Convert an unrefined type to an existentially-quantified singleton type.
  toSing   :: DemoteRep kparam -> SomeSing kparam

-- | Convenient abbreviation for 'DemoteRep':
-- @type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)@
type Demote (a :: k) = DemoteRep ('KProxy :: KProxy k)

-- | An /existentially-quantified/ singleton. This type is useful when you want a
-- singleton type, but there is no way of knowing, at compile-time, what the type
-- index will be. To make use of this type, you will generally have to use a
-- pattern-match:
--
-- > foo :: Bool -> ...
-- > foo b = case toSing b of
-- >           SomeSing sb -> {- fancy dependently-typed code with sb -}
--
-- An example like the one above may be easier to write using 'withSomeSing'.
data SomeSing (kproxy :: KProxy k) where
  SomeSing :: Sing (a :: k) -> SomeSing ('KProxy :: KProxy k)

----------------------------------------------------------------------
---- SingInstance ----------------------------------------------------
----------------------------------------------------------------------
                  
-- | A 'SingInstance' wraps up a 'SingI' instance for explicit handling.
data SingInstance (a :: k) where
  SingInstance :: SingI a => SingInstance a

-- dirty implementation of explicit-to-implicit conversion
newtype DI a = Don'tInstantiate (SingI a => SingInstance a)

-- | Get an implicit singleton (a 'SingI' instance) from an explicit one.
singInstance :: forall (a :: k). Sing a -> SingInstance a
singInstance s = with_sing_i SingInstance
  where
    with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
    with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

----------------------------------------------------------------------
---- Convenience -----------------------------------------------------
----------------------------------------------------------------------

-- | Convenience function for creating a context with an implicit singleton
-- available.
withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

-- | Convert a normal datatype (like 'Bool') to a singleton for that datatype,
-- passing it into a continuation.
withSomeSing :: SingKind ('KProxy :: KProxy k)
             => DemoteRep ('KProxy :: KProxy k)   -- ^ The original datatype
             -> (forall (a :: k). Sing a -> r)    -- ^ Function expecting a singleton
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'

-- | A convenience function useful when we need to name a singleton value
-- multiple times. Without this function, each use of 'sing' could potentially
-- refer to a different singleton, and one has to use type signatures (often
-- with @ScopedTypeVariables@) to ensure that they are the same.
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

-- | A convenience function that names a singleton satisfying a certain
-- property.  If the singleton does not satisfy the property, then the function
-- returns 'Nothing'. The property is expressed in terms of the underlying
-- representation of the singleton.
singThat :: forall (a :: k). (SingKind ('KProxy :: KProxy k), SingI a)
         => (Demote a -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

-- | Allows creation of a singleton when a proxy is at hand.
singByProxy :: SingI a => proxy a -> Sing a
singByProxy _ = sing

#if __GLASGOW_HASKELL__ >= 707
-- | Allows creation of a singleton when a @proxy#@ is at hand.
singByProxy# :: SingI a => Proxy# a -> Sing a
singByProxy# _ = sing
#endif

-- | GHC 7.8 sometimes warns about incomplete pattern matches when no such
-- patterns are possible, due to GADT constraints.
-- See the bug report at <https://ghc.haskell.org/trac/ghc/ticket/3927>.
-- In such cases, it's useful to have a catch-all pattern that then has
-- 'bugInGHC' as its right-hand side.
bugInGHC :: forall a. a
bugInGHC = error "Bug encountered in GHC -- this should never happen"

