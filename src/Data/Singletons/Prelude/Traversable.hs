{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Traversable
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'Traversable' type class.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Traversable (
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
import Data.Functor.Identity
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Singletons.Internal
import Data.Singletons.Prelude.Base hiding (Const, ConstSym0)
import Data.Singletons.Prelude.Const
import Data.Singletons.Prelude.Foldable (PFoldable, SFoldable)
import Data.Singletons.Prelude.Functor
import Data.Singletons.Prelude.Identity
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.List.Internal.Disambiguation
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Single

newtype StateL s a = StateL (s ~> (s, a))
data instance Sing :: forall s a. StateL s a -> Type where
  SStateL :: Sing x -> Sing ('StateL x)
data StateLSym0 :: forall s a. (s ~> (s, a)) ~> StateL s a
type instance Apply StateLSym0 x = 'StateL x

newtype StateR s a = StateR (s ~> (s, a))
data instance Sing :: forall s a. StateR s a -> Type where
  SStateR :: Sing x -> Sing ('StateR x)
data StateRSym0 :: forall s a. (s ~> (s, a)) ~> StateR s a
type instance Apply StateRSym0 x = 'StateR x

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
  class (Functor t, Foldable t) => Traversable (t :: Type -> Type) where
      -- {-# MINIMAL traverse | sequenceA #-}

      -- -| Map each element of a structure to an action, evaluate these actions
      -- from left to right, and collect the results. For a version that ignores
      -- the results see 'Data.Foldable.traverse_'.
      traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
      traverse f = sequenceA . fmap f

      -- -| Evaluate each action in the structure from left to right, and
      -- and collect the results. For a version that ignores the results
      -- see 'Data.Foldable.sequenceA_'.
      sequenceA :: Applicative f => (t :: Type -> Type) (f a) -> f (t a)
      sequenceA = traverse id

      -- -| Map each element of a structure to a monadic action, evaluate
      -- these actions from left to right, and collect the results. For
      -- a version that ignores the results see 'Data.Foldable.mapM_'.
      mapM :: Monad m => (a -> m b) -> t a -> m (t b)
      mapM = traverse

      -- -| Evaluate each monadic action in the structure from left to
      -- right, and collect the results. For a version that ignores the
      -- results see 'Data.Foldable.sequence_'.
      sequence :: Monad m => (t :: Type -> Type) (m a) -> m (t a)
      sequence = sequenceA

  -- instances for Prelude types

  -- deriving instance Traversable Maybe
  instance Traversable Maybe where
      traverse _ Nothing = pure Nothing
      traverse f (Just x) = Just <$> f x

  -- deriving instance Traversable []
  instance Traversable [] where
      traverse f = listfoldr cons_f (pure [])
        where cons_f x ys = liftA2 (:) (f x) ys

  -- deriving instance Traversable NonEmpty
  instance Traversable NonEmpty where
    traverse f (a :| as) = liftA2 (:|) (f a) (traverse f as)

  -- deriving instance Traversable (Either a)
  instance Traversable (Either a) where
      traverse _ (Left x) = pure (Left x)
      traverse f (Right y) = Right <$> f y

  -- deriving instance Traversable ((,) a)
  instance Traversable ((,) a) where
      traverse f (x, y) = (,) x <$> f y

  -- deriving instance Traversable (Const m)
  instance Traversable (Const m) where
      traverse _ (Const m) = pure $ Const m

  -- deriving instance Traversable Dual
  instance Traversable Dual where
      traverse f (Dual x) = Dual <$> f x

  -- deriving instance Traversable Sum
  instance Traversable Sum where
      traverse f (Sum x) = Sum <$> f x

  -- deriving instance Traversable Product
  instance Traversable Product where
      traverse f (Product x) = Product <$> f x

  -- deriving instance Traversable First
  instance Traversable First where
      traverse f (First x) = First <$> traverse f x

  -- deriving instance Traversable Last
  instance Traversable Last where
      traverse f (Last x) = Last <$> traverse f x

  -- deriving instance Traversable Identity
  instance Traversable Identity where
      traverse f (Identity x) = Identity <$> f x

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
  mapAccumL f s t = case traverse (StateL . flip f) t of
                      StateL g -> g s

  -- -|The 'mapAccumR' function behaves like a combination of 'fmap'
  -- and 'foldr'; it applies a function to each element of a structure,
  -- passing an accumulating parameter from right to left, and returning
  -- a final value of this accumulator together with the new structure.
  mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
  mapAccumR f s t = case traverse (StateR . flip f) t of
                      StateR g -> g s

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
