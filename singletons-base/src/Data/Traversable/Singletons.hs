{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable.Singletons
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'Traversable' type class.
--
----------------------------------------------------------------------------

module Data.Traversable.Singletons (
  PTraversable(..), STraversable(..),
  For, sFor,
  ForM, sForM,
  MapAccumL, sMapAccumL,
  MapAccumR, sMapAccumR,
  FmapDefault, sFmapDefault,
  FoldMapDefault, sFoldMapDefault,

  -- * Defunctionalization symbols
  TraverseSym0, TraverseSym1, TraverseSym2,
  SequenceASym0, SequenceASym1,
  MapMSym0, MapMSym1, MapMSym2,
  SequenceSym0, SequenceSym1,

  ForSym0, ForSym1, ForSym2,
  ForMSym0, ForMSym1, ForMSym2,
  MapAccumLSym0, MapAccumLSym1, MapAccumLSym2, MapAccumLSym3,
  MapAccumRSym0, MapAccumRSym1, MapAccumRSym2, MapAccumRSym3,
  FmapDefaultSym0, FmapDefaultSym1, FmapDefaultSym2,
  FoldMapDefaultSym0, FoldMapDefaultSym1, FoldMapDefaultSym2
  ) where

import Control.Applicative
import Control.Monad.Singletons.Internal
import Data.Foldable.Singletons (SFoldable)
import Data.Functor.Const.Singletons
import Data.Functor.Identity
import Data.Functor.Identity.Singletons
import Data.Functor.Singletons ()
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Monoid.Singletons
import Data.Proxy.Singletons
import Data.Singletons
import Data.Singletons.Base.Instances
import Data.Singletons.TH
import GHC.Base.Singletons hiding (Const, ConstSym0)

type StateL :: Type -> Type -> Type
newtype StateL s a = StateL (s ~> (s, a))
type SStateL :: forall s a. StateL s a -> Type
data SStateL state where
  SStateL :: Sing x -> SStateL ('StateL x)
type instance Sing @(StateL s a) = SStateL
type StateLSym0 :: forall s a. (s ~> (s, a)) ~> StateL s a
data StateLSym0 z
type instance Apply StateLSym0 x = 'StateL x

type StateR :: Type -> Type -> Type
newtype StateR s a = StateR (s ~> (s, a))
type SStateR :: forall s a. StateR s a -> Type
data SStateR state where
  SStateR :: Sing x -> SStateR ('StateR x)
type instance Sing @(StateR s a) = SStateR
type StateRSym0 :: forall s a. (s ~> (s, a)) ~> StateR s a
data StateRSym0 z
type instance Apply StateRSym0 x = 'StateR x

$(singletonsOnly [d|
  runStateL :: StateL s a -> (s -> (s, a))
  runStateL (StateL x) = x

  runStateR :: StateR s a -> (s -> (s, a))
  runStateR (StateR x) = x
  |])

$(singletonsOnly [d|
  -- -| Functors representing data structures that can be traversed from
  -- left to right.
  --
  -- A definition of 'traverse' must satisfy the following laws:
  --
  -- [/naturality/]
  --   @t . 'traverse' f = 'traverse' (t . f)@
  --   for every applicative transformation @t@
  --
  -- [/identity/]
  --   @'traverse' Identity = Identity@
  --
  -- [/composition/]
  --   @'traverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('traverse' g) . 'traverse' f@
  --
  -- A definition of 'sequenceA' must satisfy the following laws:
  --
  -- [/naturality/]
  --   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
  --   for every applicative transformation @t@
  --
  -- [/identity/]
  --   @'sequenceA' . 'fmap' Identity = Identity@
  --
  -- [/composition/]
  --   @'sequenceA' . 'fmap' Compose = Compose . 'fmap' 'sequenceA' . 'sequenceA'@
  --
  -- where an /applicative transformation/ is a function
  --
  -- @t :: (Applicative f, Applicative g) => f a -> g a@
  --
  -- preserving the 'Applicative' operations, i.e.
  --
  --  * @t ('pure' x) = 'pure' x@
  --
  --  * @t (x '<*>' y) = t x '<*>' t y@
  --
  -- and the identity functor @Identity@ and composition of functors @Compose@
  -- are defined as
  --
  -- >   newtype Identity a = Identity a
  -- >
  -- >   instance Functor Identity where
  -- >     fmap f (Identity x) = Identity (f x)
  -- >
  -- >   instance Applicative Identity where
  -- >     pure x = Identity x
  -- >     Identity f <*> Identity x = Identity (f x)
  -- >
  -- >   newtype Compose f g a = Compose (f (g a))
  -- >
  -- >   instance (Functor f, Functor g) => Functor (Compose f g) where
  -- >     fmap f (Compose x) = Compose (fmap (fmap f) x)
  -- >
  -- >   instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- >     pure x = Compose (pure (pure x))
  -- >     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
  --
  -- (The naturality law is implied by parametricity.)
  --
  -- Instances are similar to 'Functor', e.g. given a data type
  --
  -- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  --
  -- a suitable instance would be
  --
  -- > instance Traversable Tree where
  -- >    traverse f Empty = pure Empty
  -- >    traverse f (Leaf x) = Leaf <$> f x
  -- >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
  --
  -- This is suitable even for abstract types, as the laws for '<*>'
  -- imply a form of associativity.
  --
  -- The superclass instances should satisfy the following:
  --
  --  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
  --    with the identity applicative functor ('fmapDefault').
  --
  --  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
  --    equivalent to traversal with a constant applicative functor
  --    ('foldMapDefault').
  --

  -- See Note [Using standalone kind signatures not present in the base library]
  -- in Control.Monad.Singletons.Internal.
  type Traversable :: (Type -> Type) -> Constraint
  class (Functor t, Foldable t) => Traversable t where
      -- {-# MINIMAL traverse | sequenceA #-}

      -- -| Map each element of a structure to an action, evaluate these actions
      -- from left to right, and collect the results. For a version that ignores
      -- the results see 'Data.Foldable.traverse_'.
      traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
      traverse f = sequenceA . fmap f

      -- -| Evaluate each action in the structure from left to right, and
      -- and collect the results. For a version that ignores the results
      -- see 'Data.Foldable.sequenceA_'.
      sequenceA :: Applicative f => t (f a) -> f (t a)
      sequenceA = traverse id

      -- -| Map each element of a structure to a monadic action, evaluate
      -- these actions from left to right, and collect the results. For
      -- a version that ignores the results see 'Data.Foldable.mapM_'.
      mapM :: Monad m => (a -> m b) -> t a -> m (t b)
      mapM = traverse

      -- -| Evaluate each monadic action in the structure from left to
      -- right, and collect the results. For a version that ignores the
      -- results see 'Data.Foldable.sequence_'.
      sequence :: Monad m => t (m a) -> m (t a)
      sequence = sequenceA
  |])

$(singletonsOnly [d|
  -- instances for Prelude types

  deriving instance Traversable Maybe
  deriving instance Traversable []
  deriving instance Traversable NonEmpty
  deriving instance Traversable (Either a)
  deriving instance Traversable ((,) a)

  instance Traversable Proxy where
      traverse _ _ = pure Proxy
      sequenceA _ = pure Proxy
      mapM _ _ = pure Proxy
      sequence _ = pure Proxy

  deriving instance Traversable (Const m)
  deriving instance Traversable Dual
  deriving instance Traversable Sum
  deriving instance Traversable Product
  deriving instance Traversable First
  deriving instance Traversable Last
  deriving instance Traversable Identity

  -- general functions

  -- -| 'for' is 'traverse' with its arguments flipped. For a version
  -- that ignores the results see 'Data.Foldable.for_'.
  for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
  for = flip traverse

  -- -| 'forM' is 'mapM' with its arguments flipped. For a version that
  -- ignores the results see 'Data.Foldable.forM_'.
  forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
  forM = flip mapM

  instance Functor (StateL s) where
      fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

  instance Applicative (StateL s) where
      pure x = StateL (\ s -> (s, x))
      StateL kf <*> StateL kv = StateL $ \ s ->
          let (s', f) = kf s
              (s'', v) = kv s'
          in (s'', f v)
      liftA2 f (StateL kx) (StateL ky) = StateL $ \s ->
          let (s', x) = kx s
              (s'', y) = ky s'
          in (s'', f x y)

  instance Functor (StateR s) where
      fmap f (StateR k) = StateR $ \ s -> let (s', v) = k s in (s', f v)

  instance Applicative (StateR s) where
      pure x = StateR (\ s -> (s, x))
      StateR kf <*> StateR kv = StateR $ \ s ->
          let (s', v) = kv s
              (s'', f) = kf s'
          in (s'', f v)
      liftA2 f (StateR kx) (StateR ky) = StateR $ \ s ->
          let (s', y) = ky s
              (s'', x) = kx s'
          in (s'', f x y)

  -- -|The 'mapAccumL' function behaves like a combination of 'fmap'
  -- and 'foldl'; it applies a function to each element of a structure,
  -- passing an accumulating parameter from left to right, and returning
  -- a final value of this accumulator together with the new structure.
  mapAccumL :: forall t a b c. Traversable t
            => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
  mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s

  -- -|The 'mapAccumR' function behaves like a combination of 'fmap'
  -- and 'foldr'; it applies a function to each element of a structure,
  -- passing an accumulating parameter from right to left, and returning
  -- a final value of this accumulator together with the new structure.
  mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
  mapAccumR f s t = runStateR (traverse (StateR . flip f) t) s

  -- -| This function may be used as a value for `fmap` in a `Functor`
  --   instance, provided that 'traverse' is defined. (Using
  --   `fmapDefault` with a `Traversable` instance defined only by
  --   'sequenceA' will result in infinite recursion.)
  --
  -- @
  -- 'fmapDefault' f ≡ 'runIdentity' . 'traverse' ('Identity' . f)
  -- @
  fmapDefault :: forall t a b . Traversable t
              => (a -> b) -> t a -> t b
  fmapDefault f x = case traverse (Identity . f) x of Identity y -> y

  -- -| This function may be used as a value for `Data.Foldable.foldMap`
  -- in a `Foldable` instance.
  --
  -- @
  -- 'foldMapDefault' f ≡ 'getConst' . 'traverse' ('Const' . f)
  -- @
  foldMapDefault :: forall t m a . (Traversable t, Monoid m)
                 => (a -> m) -> t a -> m
  foldMapDefault f x = case traverse (mkConst . f) x of Const y -> y
    where
      mkConst :: m -> Const m ()
      mkConst = Const
  |])
