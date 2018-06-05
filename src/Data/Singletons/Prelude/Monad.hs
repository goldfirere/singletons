{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Monad
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the promoted and singled versions of the 'Monad' type class.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Monad (
  PFunctor(Fmap), SFunctor(sFmap),
  PMonad(..), SMonad(..), PMonadPlus(..), SMonadPlus(..),

  {-
  MapM, sMapM, MapM_, sMapM_, ForM, sForM,
  Sequence, sSequence, Sequence_, sSequence_,
  -}
  type (=<<), (%=<<), type (>=>), (%>=>), type (<=<), (%<=<),
  -- Forever, sForever,
  Void, sVoid,

  Join, sJoin,
  -- Msum, sMsum,
  Mfilter, sMfilter, FilterM, sFilterM,
  {-
  MapAndUnzipM, sMapAndUnzipM, ZipWithM, sZipWithM,
  ZipWithM_, sZipWithM_, FoldlM, sFoldlM,
  -}
  ReplicateM, sReplicateM, ReplicateM_, sReplicateM_,

  Guard, sGuard, When, sWhen, Unless, sUnless,

  LiftM, sLiftM, LiftM2, sLiftM2, LiftM3, sLiftM3,
  LiftM4, sLiftM4, LiftM5, sLiftM5, Ap, sAp,

  type (<$!>), (%<$!>),

  -- * Defunctionalization symbols
  FmapSym0, FmapSym1, FmapSym2,
  type (>>=@#@$), type (>>=@#@$$), type (>>=@#@$$$),
  type (>>@#@$),  type (>>@#@$$),  type (>>@#@$$$),
  ReturnSym0, ReturnSym1, FailSym0, FailSym1,
  MzeroSym0, MplusSym0, MplusSym1, MplusSym2,

  {-
  MapMSym0,  MapMSym1,  MapMSym2,
  MapM_Sym0, MapM_Sym1, MapM_Sym2,
  ForMSym0,  ForMSym1,  ForMSym2,
  SequenceSym0,  SequenceSym1,
  Sequence_Sym0, Sequence_Sym1,
  -}
  type (=<<@#@$), type (=<<@#@$$), type (=<<@#@$$$),
  type (>=>@#@$), type (>=>@#@$$), type (>=>@#@$$$),
  type (<=<@#@$), type (<=<@#@$$), type (<=<@#@$$$),
  -- ForeverSym0, ForeverSym1,
  VoidSym0, VoidSym1,

  JoinSym0, JoinSym1,
  -- MsumSym0, MsumSym1,
  MfilterSym0, MfilterSym1, MfilterSym2,
  FilterMSym0, FilterMSym1, FilterMSym2,
  {-
  MapAndUnzipMSym0, MapAndUnzipMSym1, MapAndUnzipMSym2,
  ZipWithMSym0,  ZipWithMSym1,  ZipWithMSym2,  ZipWithMSym3,
  ZipWithM_Sym0, ZipWithM_Sym1, ZipWithM_Sym2, ZipWithM_Sym3,
  FoldlMSym0,    FoldlMSym1,    FoldlMSym2,    FoldlMSym3,
  -}
  ReplicateMSym0,  ReplicateMSym1,  ReplicateMSym2,
  ReplicateM_Sym0, ReplicateM_Sym1, ReplicateM_Sym2,

  GuardSym0, GuardSym1,
  WhenSym0, WhenSym1, WhenSym2,
  UnlessSym0, UnlessSym1, UnlessSym2,

  LiftMSym0,  LiftMSym1,  LiftMSym2,
  LiftM2Sym0, LiftM2Sym1, LiftM2Sym2, LiftM2Sym3,
  LiftM3Sym0, LiftM3Sym1, LiftM3Sym2, LiftM3Sym3, LiftM3Sym4,
  LiftM4Sym0, LiftM4Sym1, LiftM4Sym2, LiftM4Sym3, LiftM4Sym4, LiftM4Sym5,
  LiftM5Sym0, LiftM5Sym1, LiftM5Sym2, LiftM5Sym3, LiftM5Sym4, LiftM5Sym5, LiftM5Sym6,
  ApSym0, ApSym1, ApSym2,

  type (<$!>@#@$), type (<$!>@#@$$), type (<$!>@#@$$$),
  ) where

import Control.Applicative
import Control.Monad
import Data.Ord (Down(..))
import Data.Singletons.Prelude.Applicative ()
import Data.Singletons.Prelude.Base
import Data.Singletons.Prelude.Functor
import Data.Singletons.Prelude.Instances
import Data.Singletons.Prelude.Monad.Internal
import Data.Singletons.Prelude.Monoid
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.Ord
import Data.Singletons.Single
import GHC.TypeNats

$(singletonsOnly [d|
  -- -----------------------------------------------------------------------------
  -- Functions mandated by the Prelude

  -- -| @'guard' b@ is @'pure' ()@ if @b@ is 'True',
  -- and 'empty' if @b@ is 'False'.
  guard           :: (Alternative f) => Bool -> f ()
  guard True      =  pure ()
  guard False     =  empty

  -- -| This generalizes the list-based 'filter' function.

  filterM          :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
  filterM p        = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])

  infixr 1 <=<, >=>

  -- -| Left-to-right Kleisli composition of monads.
  (>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
  f >=> g     = \x -> f x >>= g

  -- -| Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped.
  --
  -- Note how this operator resembles function composition @('.')@:
  --
  -- > (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
  -- > (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
  (<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
  (<=<)       = flip (>=>)

  {-
  Relies on infinite lists

  -- -| @'forever' act@ repeats the action infinitely.
  forever     :: (Applicative f) => (f :: Type -> Type) a -> f b
  forever a   = let a' = a *> a' in a'
  -- Use explicit sharing here, as it prevents a space leak regardless of
  -- optimizations.
  -}

  -- -----------------------------------------------------------------------------
  -- Other monad functions

  {-
  -- -| The 'mapAndUnzipM' function maps its first argument over a list, returning
  -- the result as a pair of lists. This function is mainly used with complicated
  -- data structures or a state-transforming monad.
  mapAndUnzipM      :: (Applicative m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
  mapAndUnzipM f xs =  unzip <$> traverse f xs

  -- -| The 'zipWithM' function generalizes 'zipWith' to arbitrary applicative functors.
  zipWithM          :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
  zipWithM f xs ys  =  sequenceA (zipWith f xs ys)

  -- -| 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
  zipWithM_         :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
  zipWithM_ f xs ys =  sequenceA_ (zipWith f xs ys)

  {- -| The 'foldM' function is analogous to 'foldl', except that its result is
  encapsulated in a monad. Note that 'foldM' works from left-to-right over
  the list arguments. This could be an issue where @('>>')@ and the `folded
  function' are not commutative.


  >       foldM f a1 [x1, x2, ..., xm]

  ==

  >       do
  >         a2 <- f a1 x1
  >         a3 <- f a2 x2
  >         ...
  >         f am xm

  If right-to-left evaluation is required, the input list should be reversed.

  Note: 'foldM' is the same as 'foldlM'
  -}

  foldM          :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  foldM          = foldlM

  -- -| Like 'foldM', but discards the result.
  foldM_         :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
  foldM_ f a xs  = foldlM f a xs >> return ()
  -}

  {-
  Note [Worker/wrapper transform on replicateM/replicateM_]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The implementations of replicateM and replicateM_ both leverage the
  worker/wrapper transform. The simpler implementation of replicateM_, as an
  example, would be:

      replicateM_ 0 _ = pure ()
      replicateM_ n f = f *> replicateM_ (n - 1) f

  However, the self-recursive nature of this implementation inhibits inlining,
  which means we never get to specialise to the action (`f` in the code above).
  By contrast, the implementation below with a local loop makes it possible to
  inline the entire definition (as happens for foldr, for example) thereby
  specialising for the particular action.

  For further information, see this Trac comment, which includes side-by-side
  Core: https://ghc.haskell.org/trac/ghc/ticket/11795#comment:6
  -}

  -- -| @'replicateM' n act@ performs the action @n@ times,
  -- gathering the results.
  replicateM        :: (Applicative m) => Nat -> m a -> m [a]
  replicateM cnt0 f =
      loop cnt0
    where
      loop cnt
          | cnt <= 0  = pure []
          | otherwise = liftA2 (:) f (loop (cnt - 1))

  -- -| Like 'replicateM', but discards the result.
  replicateM_       :: (Applicative m) => Nat -> m a -> m ()
  replicateM_ cnt0 f =
      loop cnt0
    where
      loop cnt
          | cnt <= 0  = pure ()
          | otherwise = f *> loop (cnt - 1)


  -- -| The reverse of 'when'.
  unless            :: (Applicative f) => Bool -> f () -> f ()
  unless p s        =  if p then pure () else s

  infixl 4 <$!>

  -- -| Strict version of 'Data.Functor.<$>'.
  --
  -- @since 4.8.0.0
  (<$!>) :: Monad m => (a -> b) -> m a -> m b
  f <$!> m = do
    x <- m
    let z = f x
    z `seq` return z


  -- -----------------------------------------------------------------------------
  -- Other MonadPlus functions

  -- -| Direct 'MonadPlus' equivalent of 'filter'
  -- @'filter'@ = @(mfilter:: (a -> Bool) -> [a] -> [a]@
  -- applicable to any 'MonadPlus', for example
  -- @mfilter odd (Just 1) == Just 1@
  -- @mfilter odd (Just 2) == Nothing@

  mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
  mfilter p ma = do
    a <- ma
    if p a then return a else mzero

  {- -$naming

  The functions in this library use the following naming conventions:

  * A postfix \'@M@\' always stands for a function in the Kleisli category:
    The monad type constructor @m@ is added to function results
    (modulo currying) and nowhere else.  So, for example,

  >  filter  ::              (a ->   Bool) -> [a] ->   [a]
  >  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

  * A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
    Thus, for example:

  >  sequence  :: Monad m => [m a] -> m [a]
  >  sequence_ :: Monad m => [m a] -> m ()

  * A prefix \'@m@\' generalizes an existing function to a monadic form.
    Thus, for example:

  >  sum  :: Num a       => [a]   -> a
  >  msum :: MonadPlus m => [m a] -> m a

  -}

  instance Monoid a => Monad ((,) a) where
      (u, a) >>= k = case k a of (v, b) -> (u `mappend` v, b)

  instance Monad Down where
    Down a >>= k = k a
  |])

-- Workaround for #326
infixr 1 <=<, >=>
infixl 4 <$!>
