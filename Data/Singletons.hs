{- Data/Singletons.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This is the public interface file to the singletons library. Please
see the accompanying README file for more information. Haddock is
not currently compatible with the features used here, so the documentation
is all in the README file and /Dependently typed programming with singletons/,
available at <http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf>
-}

{-# LANGUAGE MagicHash, RankNTypes, PolyKinds, GADTs, DataKinds,
             FlexibleContexts, CPP, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-binds #-}
-- We make unused bindings for (||), (&&), and not.

module Data.Singletons (
  KProxy(..), Proxy(..),
  Sing, SingI(..), SingKind(..), KindOf, Demote,
  SingInstance(..), SomeSing(..),
  singInstance, withSingI, withSomeSing, singByProxy,
#if __GLASGOW_HASKELL__ >= 707
  singByProxy#,
#endif
  withSing,
  bugInGHC, Error, sError
  ) where

import Data.Singletons.Types
import Data.Singletons.Core
import GHC.Exts
import Unsafe.Coerce
import GHC.TypeLits (Symbol)

-- support for converting an explicit singleton to an implicit one
data SingInstance :: k -> * where
  SingInstance :: SingI a => SingInstance a

-- dirty implementation of explicit-to-implicit conversion
newtype Don'tInstantiate a = MkDI (SingI a => SingInstance a)
singInstance :: forall (a :: k). Sing a -> SingInstance a
singInstance s = with_sing_i s SingInstance
  where
    with_sing_i :: Sing a -> (SingI a => SingInstance a) -> SingInstance a
    with_sing_i s si = unsafeCoerce (MkDI si) s

-- easy use of implicit instances
withSingI :: Sing n -> (SingI n => r) -> r
withSingI sn r =
  case singInstance sn of
    SingInstance -> r

-- refine a datatype to a singleton locally
withSomeSing :: SingKind ('KProxy :: KProxy k)
             => DemoteRep ('KProxy :: KProxy k)
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing x f =
  case toSing x of
    SomeSing x' -> f x'

{- | A convenience function useful when we need to name a singleton value
multiple times.  Without this function, each use of 'sing' could potentially
refer to a different singleton, and one has to use type signatures to
ensure that they are the same. -}
withSing :: SingI a => (Sing a -> b) -> b
withSing f = f sing

{- | A convenience function that names a singleton satisfying a certain
property.  If the singleton does not satisfy the property, then the function
returns 'Nothing'. The property is expressed in terms of the underlying
representation of the singleton. -}
singThat :: forall (a :: k). (SingKind ('KProxy :: KProxy k), SingI a)
         => (Demote a -> Bool) -> Maybe (Sing a)
singThat p = withSing $ \x -> if p (fromSing x) then Just x else Nothing

-- allows creation of a singleton when a proxy is at hand
singByProxy :: SingI a => proxy a -> Sing a
singByProxy _ = sing

#if __GLASGOW_HASKELL__ >= 707
singByProxy# :: SingI a => Proxy# a -> Sing a
singByProxy# _ = sing
#endif

-- useful when suppressing GHC's warnings about incomplete pattern matches
bugInGHC :: forall a. a
bugInGHC = error "Bug encountered in GHC -- this should never happen"

-- for promotion of `error`
type family Error (str :: Symbol) :: k

sError :: Sing (str :: Symbol) -> a
sError sstr = error (fromSing sstr)
