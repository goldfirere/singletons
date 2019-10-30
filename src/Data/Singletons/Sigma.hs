{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Sigma
-- Copyright   :  (C) 2017 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines 'Sigma', a dependent pair data type, and related functions.
--
----------------------------------------------------------------------------

module Data.Singletons.Sigma
    ( -- * The 'Sigma' type
      Sigma(..), Σ
    , Sing, SSigma(..), SΣ

      -- * Operations over 'Sigma'
    , fstSigma, FstSigma, sndSigma, SndSigma
    , projSigma1, projSigma2
    , mapSigma, zipSigma
    , currySigma, uncurrySigma

      -- * Internal utilities
      -- $internalutilities
    , ShowApply,  ShowSingApply
    , ShowApply', ShowSingApply'
    ) where

import Data.Kind
import Data.Singletons.Internal
import Data.Singletons.ShowSing

-- | A dependent pair.
type Sigma :: forall s -> (s ~> Type) -> Type
data Sigma s t where
  (:&:) :: forall s t fst. Sing (fst :: s) -> t @@ fst -> Sigma s t
infixr 4 :&:
instance (ShowSing s, ShowApply t) => Show (Sigma s t) where
  showsPrec p ((a :: Sing (fst :: s)) :&: b) = showParen (p >= 5) $
    showsPrec 5 a . showString " :&: " . showsPrec 5 b
      :: (ShowSing' fst, ShowApply' t fst) => ShowS

-- | Unicode shorthand for 'Sigma'.
type Σ :: forall s -> (s ~> Type) -> Type
type Σ = Sigma

-- | The singleton type for 'Sigma'.
type SSigma :: Sigma s t -> Type
data SSigma sig where
  (:%&:) :: forall s t (fst :: s) (sfst :: Sing fst) (snd :: t @@ fst).
            Sing ('WrapSing sfst) -> Sing snd -> SSigma (sfst ':&: snd :: Sigma s t)
infixr 4 :%&:

type instance Sing = SSigma
instance forall s (t :: s ~> Type) (sig :: Sigma s t).
         (ShowSing s, ShowSingApply t)
      => Show (SSigma sig) where
  showsPrec p ((sa :: Sing ('WrapSing (sfst :: Sing fst))) :%&: (sb :: Sing snd)) =
    showParen (p >= 5) $
      showsPrec 5 sa . showString " :&: " . showsPrec 5 sb
        :: (ShowSing' fst, ShowSingApply' t fst snd) => ShowS

instance forall s t (fst :: s) (a :: Sing fst) (b :: t @@ fst).
       (SingI fst, SingI b)
    => SingI (a ':&: b :: Sigma s t) where
  sing = sing :%&: sing

-- | Unicode shorthand for 'SSigma'.
type SΣ :: Sigma s t -> Type
type SΣ = SSigma

-- | Project the first element out of a dependent pair.
fstSigma :: forall s t. SingKind s => Sigma s t -> Demote s
fstSigma (a :&: _) = fromSing a

-- | Project the first element out of a dependent pair.
type FstSigma :: Sigma s t -> s
type family FstSigma sig where
  FstSigma ((_ :: Sing fst) ':&: _) = fst

-- | Project the second element out of a dependent pair.
sndSigma :: forall s t (sig :: Sigma s t).
            SingKind (t @@ FstSigma sig)
         => SSigma sig -> Demote (t @@ FstSigma sig)
sndSigma (_ :%&: b) = fromSing b

-- | Project the second element out of a dependent pair.
type SndSigma :: forall s t. forall (sig :: Sigma s t) -> t @@ FstSigma sig
type family SndSigma sig where
  SndSigma (_ ':&: b) = b

-- | Project the first element out of a dependent pair using
-- continuation-passing style.
projSigma1 :: (forall (fst :: s). Sing fst -> r) -> Sigma s t -> r
projSigma1 f (a :&: _) = f a

-- | Project the second element out of a dependent pair using
-- continuation-passing style.
projSigma2 :: forall s t r. (forall (fst :: s). t @@ fst -> r) -> Sigma s t -> r
projSigma2 f ((_ :: Sing (fst :: s)) :&: b) = f @fst b

-- | Map across a 'Sigma' value in a dependent fashion.
mapSigma :: Sing (f :: a ~> b) -> (forall (x :: a). p @@ x -> q @@ (f @@ x))
         -> Sigma a p -> Sigma b q
mapSigma f g ((x :: Sing (fst :: a)) :&: y) = (f @@ x) :&: (g @fst y)

-- | Zip two 'Sigma' values together in a dependent fashion.
zipSigma :: Sing (f :: a ~> b ~> c)
         -> (forall (x :: a) (y :: b). p @@ x -> q @@ y -> r @@ (f @@ x @@ y))
         -> Sigma a p -> Sigma b q -> Sigma c r
zipSigma f g ((a :: Sing (fstA :: a)) :&: p) ((b :: Sing (fstB :: b)) :&: q) =
  (f @@ a @@ b) :&: (g @fstA @fstB p q)

-- | Convert an uncurried function on 'Sigma' to a curried one.
--
-- Together, 'currySigma' and 'uncurrySigma' witness an isomorphism such that
-- the following identities hold:
--
-- @
-- id1 :: forall a (b :: a ~> Type) (c :: 'Sigma' a b ~> Type).
--        (forall (p :: Sigma a b). 'SSigma' p -> c @@ p)
--     -> (forall (p :: Sigma a b). 'SSigma' p -> c @@ p)
-- id1 f = 'uncurrySigma' @a @b @c ('currySigma' @a @b @c f)
--
-- id2 :: forall a (b :: a ~> Type) (c :: 'Sigma' a b ~> Type).
--        (forall (x :: a) (sx :: Sing x) (y :: b @@ x). Sing ('WrapSing' sx) -> Sing y -> c @@ (sx :&: y))
--     -> (forall (x :: a) (sx :: Sing x) (y :: b @@ x). Sing ('WrapSing' sx) -> Sing y -> c @@ (sx :&: y))
-- id2 f = 'currySigma' @a @b @c ('uncurrySigma' @a @b @c f)
-- @
currySigma :: forall a (b :: a ~> Type) (c :: Sigma a b ~> Type).
              (forall (p :: Sigma a b). SSigma p -> c @@ p)
           -> (forall (x :: a) (sx :: Sing x) (y :: b @@ x).
                 Sing ('WrapSing sx) -> Sing y -> c @@ (sx ':&: y))
currySigma f x y = f (x :%&: y)

-- | Convert a curried function on 'Sigma' to an uncurried one.
--
-- Together, 'currySigma' and 'uncurrySigma' witness an isomorphism.
-- (Refer to the documentation for 'currySigma' for more details.)
uncurrySigma :: forall a (b :: a ~> Type) (c :: Sigma a b ~> Type).
                (forall (x :: a) (sx :: Sing x) (y :: b @@ x).
                   Sing ('WrapSing sx) -> Sing y -> c @@ (sx ':&: y))
             -> (forall (p :: Sigma a b). SSigma p -> c @@ p)
uncurrySigma f (x :%&: y) = f x y

------------------------------------------------------------
-- Internal utilities
------------------------------------------------------------

{- $internal-utilities

See the documentation in "Data.Singletons.ShowSing"—in particular, the
Haddocks for 'ShowSing' and `ShowSing'`—for an explanation for why these
classes exist.
-}

type ShowApply :: (a ~> Type) -> Constraint
class    (forall (x :: a). ShowApply' f x) => ShowApply (f :: a ~> Type)
instance (forall (x :: a). ShowApply' f x) => ShowApply (f :: a ~> Type)

type ShowApply' :: (a ~> Type) -> a -> Constraint
class    Show (Apply f x) => ShowApply' (f :: a ~> Type) (x :: a)
instance Show (Apply f x) => ShowApply' (f :: a ~> Type) (x :: a)

type ShowSingApply :: (a ~> Type) -> Constraint
class    (forall (x :: a) (z :: Apply f x). ShowSingApply' f x z) => ShowSingApply (f :: a ~> Type)
instance (forall (x :: a) (z :: Apply f x). ShowSingApply' f x z) => ShowSingApply (f :: a ~> Type)

type ShowSingApply' :: forall a. forall (f :: a ~> Type) (x :: a) -> Apply f x -> Constraint
class    Show (Sing z) => ShowSingApply' (f :: a ~> Type) (x :: a) (z :: Apply f x)
instance Show (Sing z) => ShowSingApply' (f :: a ~> Type) (x :: a) (z :: Apply f x)
