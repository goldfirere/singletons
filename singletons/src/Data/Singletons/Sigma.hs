{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#else
{-# LANGUAGE TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#else
{-# LANGUAGE ImpredicativeTypes #-} -- See Note [Impredicative Σ?]
#endif

#if __GLASGOW_HASKELL__ >= 811
{-# LANGUAGE UnsaturatedTypeFamilies #-}
#endif

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

#if __GLASGOW_HASKELL__ >= 806
      -- * Internal utilities
      -- $internalutilities
    , ShowApply,  ShowSingApply
    , ShowApply', ShowSingApply'
#endif
    ) where

import Data.Kind
import Data.Singletons
#if __GLASGOW_HASKELL__ >= 806
import Data.Singletons.ShowSing
#endif

-- | A dependent pair.
#if __GLASGOW_HASKELL__ >= 811
type Sigma :: forall {m}. forall s -> (s -> @m Type) -> Type
data Sigma (s :: Type) :: (s -> @m Type) -> Type where
  (:&:) :: forall s t fst. Sing (fst :: s) -> t fst -> Sigma s t
#else
#if __GLASGOW_HASKELL__ >= 810
type Sigma :: forall s -> (s ~> Type) -> Type
#endif
data Sigma (s :: Type) :: (s ~> Type) -> Type where
  (:&:) :: forall s t fst. Sing (fst :: s) -> t @@ fst -> Sigma s t
#endif
infixr 4 :&:

-- | Unicode shorthand for 'Sigma'.
#if __GLASGOW_HASKELL__ >= 811
type Σ :: forall {m}. forall s -> (s -> @m Type) -> Type
#elif __GLASGOW_HASKELL__ >= 810
type Σ :: forall s -> (s ~> Type) -> Type
#endif
type Σ = Sigma

{-
Note [Impredicative Σ?]
~~~~~~~~~~~~~~~~~~~~~~~
The following definition alone:

  type Σ = Sigma

will not typecheck without the use of ImpredicativeTypes. There isn't a
fundamental reason that this should be the case, and the only reason that GHC
currently requires this is due to GHC#13408. Thankfully, giving Σ a standalone
kind signature works around GHC#13408, so we only have to enable
ImpredicativeTypes on pre-8.10 versions of GHC.
-}

-- | The singleton type for 'Sigma'.
#if __GLASGOW_HASKELL__ >= 811
type SSigma :: forall {m} s (t :: s -> @m Type). Sigma s t -> Type
data SSigma :: forall {m} s (t :: s -> @m Type). Sigma s t -> Type where
  (:%&:) :: forall s t (fst :: s) (sfst :: Sing fst) (snd :: t fst).
            Sing ('WrapSing sfst) -> Sing snd -> SSigma (sfst ':&: snd :: Sigma s t)
#else
#if __GLASGOW_HASKELL__ >= 810
type SSigma :: forall s (t :: s ~> Type). Sigma s t -> Type
#endif
data SSigma :: forall s t. Sigma s t -> Type where
  (:%&:) :: forall s t (fst :: s) (sfst :: Sing fst) (snd :: t @@ fst).
            Sing ('WrapSing sfst) -> Sing snd -> SSigma (sfst ':&: snd :: Sigma s t)
#endif
infixr 4 :%&:
type instance Sing = SSigma

#if __GLASGOW_HASKELL__ >= 811
instance forall m s (t :: s -> @m Type) (fst :: s) (a :: Sing fst) (b :: t fst).
#else
instance forall s (t :: s ~> Type) (fst :: s) (a :: Sing fst) (b :: t @@ fst).
#endif
       (SingI fst, SingI b)
    => SingI (a ':&: b :: Sigma s t) where
  sing = sing :%&: sing

-- | Unicode shorthand for 'SSigma'.
#if __GLASGOW_HASKELL__ >= 811
type SΣ :: forall {m} s (t :: s -> @m Type). Sigma s t -> Type
#elif __GLASGOW_HASKELL__ >= 810
type SΣ :: forall s (t :: s ~> Type). Sigma s t -> Type
#endif
type SΣ = SSigma

-- | Project the first element out of a dependent pair.
fstSigma ::
#if __GLASGOW_HASKELL__ >= 811
  forall {m} s (t :: s -> @m Type).
#else
  forall s (t :: s ~> Type).
#endif
  SingKind s => Sigma s t -> Demote s
fstSigma (a :&: _) = fromSing a

-- | Project the first element out of a dependent pair.
#if __GLASGOW_HASKELL__ >= 811
type FstSigma :: forall {m} s (t :: s -> @m Type). Sigma s t -> s
#elif __GLASGOW_HASKELL__ >= 810
type FstSigma :: forall s (t :: s ~> Type). Sigma s t -> s
#endif
type family FstSigma (sig :: Sigma s t) :: s where
  FstSigma ((_ :: Sing fst) ':&: _) = fst

-- | Project the second element out of a dependent pair.
sndSigma ::
#if __GLASGOW_HASKELL__ >= 811
     forall {m} s (t :: s -> @m Type) (sig :: Sigma s t). SingKind (t (FstSigma sig))
  => SSigma sig -> Demote (t (FstSigma sig))
#else
     forall s (t :: s ~> Type) (sig :: Sigma s t). SingKind (t @@ FstSigma sig)
  => SSigma sig -> Demote (t @@ FstSigma sig)
#endif
sndSigma (_ :%&: b) = fromSing b

-- | Project the second element out of a dependent pair.
#if __GLASGOW_HASKELL__ >= 811
type SndSigma :: forall {m} s (t :: s -> @m Type). forall (sig :: Sigma s t) -> t (FstSigma sig)
type family SndSigma (sig :: Sigma s t) :: t (FstSigma sig) where
  SndSigma (_ ':&: b) = b
#else
#if __GLASGOW_HASKELL__ >= 810
type SndSigma :: forall s (t :: s ~> Type). forall (sig :: Sigma s t) -> t @@ FstSigma sig
#endif
type family SndSigma (sig :: Sigma s t) :: t @@ FstSigma sig where
  SndSigma (_ ':&: b) = b
#endif

-- | Project the first element out of a dependent pair using
-- continuation-passing style.
projSigma1 ::
#if __GLASGOW_HASKELL__ >= 811
  forall {m} s (t :: s -> @m Type) r.
#else
  forall s (t :: s ~> Type) r.
#endif
  (forall (fst :: s). Sing fst -> r) -> Sigma s t -> r
projSigma1 f (a :&: _) = f a

-- | Project the second element out of a dependent pair using
-- continuation-passing style.
projSigma2 ::
#if __GLASGOW_HASKELL__ >= 811
  forall {m} s (t :: s -> @m Type) r.
  (forall (fst :: s). t fst -> r) -> Sigma s t -> r
#else
  forall s (t :: s ~> Type) r.
  (forall (fst :: s). t @@ fst -> r) -> Sigma s t -> r
#endif
projSigma2 f ((_ :: Sing (fst :: s)) :&: b) = f @fst b

-- | Map across a 'Sigma' value in a dependent fashion.
mapSigma ::
#if __GLASGOW_HASKELL__ >= 811
     forall {m} {n} {o} a b (p :: a -> @m Type) (q :: b -> @n Type) (f :: a -> @o b).
     Sing f -> (forall (x :: a). p x -> q (f x))
  -> Sigma a p -> Sigma b q
#else
     forall a b (p :: a ~> Type) (q :: b ~> Type) (f :: a ~> b).
     Sing (f :: a ~> b) -> (forall (x :: a). p @@ x -> q @@ (f @@ x))
  -> Sigma a p -> Sigma b q
#endif
mapSigma f g ((x :: Sing (fst :: a)) :&: y) = (f @@ x) :&: (g @fst y)

-- | Zip two 'Sigma' values together in a dependent fashion.
zipSigma ::
#if __GLASGOW_HASKELL__ >= 811
     forall {j} {k} {l} {m} {n} a b c
            (p :: a -> @j Type) (q :: b -> @k Type) (r :: c -> @l Type)
            (f :: a -> @m b -> @n c).
     Sing f
  -> (forall (x :: a) (y :: b). p x -> q y -> r (f x y))
  -> Sigma a p -> Sigma b q -> Sigma c r
#else
     forall a b c
            (p :: a ~> Type) (q :: b ~> Type) (r :: c ~> Type)
            (f :: a ~> b ~> c).
     Sing (f :: a ~> b ~> c)
  -> (forall (x :: a) (y :: b). p @@ x -> q @@ y -> r @@ (f @@ x @@ y))
  -> Sigma a p -> Sigma b q -> Sigma c r
#endif
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
currySigma ::
#if __GLASGOW_HASKELL__ >= 811
     forall {m} {n} a (b :: a -> @m Type) (c :: Sigma a b -> @n Type).
     (forall (p :: Sigma a b). SSigma p -> c p)
  -> (forall (x :: a) (sx :: Sing x) (y :: b x).
        Sing ('WrapSing sx) -> Sing y -> c (sx ':&: y))
#else
     forall a (b :: a ~> Type) (c :: Sigma a b ~> Type).
     (forall (p :: Sigma a b). SSigma p -> c @@ p)
  -> (forall (x :: a) (sx :: Sing x) (y :: b @@ x).
        Sing ('WrapSing sx) -> Sing y -> c @@ (sx ':&: y))
#endif
currySigma f x y = f (x :%&: y)

-- | Convert a curried function on 'Sigma' to an uncurried one.
--
-- Together, 'currySigma' and 'uncurrySigma' witness an isomorphism.
-- (Refer to the documentation for 'currySigma' for more details.)
uncurrySigma ::
#if __GLASGOW_HASKELL__ >= 811
     forall {m} {n} a (b :: a -> @m Type) (c :: Sigma a b -> @n Type).
     (forall (x :: a) (sx :: Sing x) (y :: b x).
        Sing ('WrapSing sx) -> Sing y -> c (sx ':&: y))
  -> (forall (p :: Sigma a b). SSigma p -> c p)
#else
     forall a (b :: a ~> Type) (c :: Sigma a b ~> Type).
     (forall (x :: a) (sx :: Sing x) (y :: b @@ x).
        Sing ('WrapSing sx) -> Sing y -> c @@ (sx ':&: y))
  -> (forall (p :: Sigma a b). SSigma p -> c @@ p)
#endif
uncurrySigma f (x :%&: y) = f x y

#if __GLASGOW_HASKELL__ >= 806
instance
#if __GLASGOW_HASKELL__ >= 811
    forall m s (t :: s -> @m Type).
#else
    forall s (t :: s ~> Type).
#endif
    (ShowSing s, ShowApply t) => Show (Sigma s t) where
  showsPrec p ((a :: Sing (fst :: s)) :&: b) = showParen (p >= 5) $
    showsPrec 5 a . showString " :&: " . showsPrec 5 b
      :: (ShowSing' fst, ShowApply' t fst) => ShowS

instance
#if __GLASGOW_HASKELL__ >= 811
       forall m s (t :: s -> @m Type) (sig :: Sigma s t).
#else
       forall s (t :: s ~> Type) (sig :: Sigma s t).
#endif
       (ShowSing s, ShowSingApply t)
    => Show (SSigma sig) where
  showsPrec p ((sa :: Sing ('WrapSing (sfst :: Sing fst))) :%&: (sb :: Sing snd)) =
    showParen (p >= 5) $
      showsPrec 5 sa . showString " :&: " . showsPrec 5 sb
        :: (ShowSing' fst, ShowSingApply' t fst snd) => ShowS

------------------------------------------------------------
-- Internal utilities
------------------------------------------------------------

{- $internal-utilities

See the documentation in "Data.Singletons.ShowSing"—in particular, the
Haddocks for 'ShowSing' and `ShowSing'`—for an explanation for why these
classes exist.

Note that these classes are only defined on GHC 8.6 or later.
-}

#if __GLASGOW_HASKELL__ >= 811
type ShowApply :: forall {m} a. (a -> @m Type) -> Constraint
class    (forall (x :: a). ShowApply' f x) => ShowApply (f :: a -> @m Type)
instance (forall (x :: a). ShowApply' f x) => ShowApply (f :: a -> @m Type)

type ShowApply' :: forall {m} a. (a -> @m Type) -> a -> Constraint
class    Show (f x) => ShowApply' (f :: a -> @m Type) (x :: a)
instance Show (f x) => ShowApply' (f :: a -> @m Type) (x :: a)

type ShowSingApply :: forall {m} a. (a -> @m Type) -> Constraint
class    (forall (x :: a) (z :: f x). ShowSingApply' f x z) => ShowSingApply (f :: a -> @m Type)
instance (forall (x :: a) (z :: f x). ShowSingApply' f x z) => ShowSingApply (f :: a -> @m Type)

type ShowSingApply' :: forall {m} a. forall (f :: a -> @m Type) (x :: a) -> f x -> Constraint
class    Show (Sing z) => ShowSingApply' (f :: a -> @m Type) (x :: a) (z :: f x)
instance Show (Sing z) => ShowSingApply' (f :: a -> @m Type) (x :: a) (z :: f x)
#else
#if __GLASGOW_HASKELL__ >= 810
type ShowApply :: forall a. (a ~> Type) -> Constraint
#endif
class    (forall (x :: a). ShowApply' f x) => ShowApply (f :: a ~> Type)
instance (forall (x :: a). ShowApply' f x) => ShowApply (f :: a ~> Type)

#if __GLASGOW_HASKELL__ >= 810
type ShowApply' :: forall a. (a ~> Type) -> a -> Constraint
#endif
class    Show (Apply f x) => ShowApply' (f :: a ~> Type) (x :: a)
instance Show (Apply f x) => ShowApply' (f :: a ~> Type) (x :: a)

#if __GLASGOW_HASKELL__ >= 810
type ShowSingApply :: forall a. (a ~> Type) -> Constraint
#endif
class    (forall (x :: a) (z :: Apply f x). ShowSingApply' f x z) => ShowSingApply (f :: a ~> Type)
instance (forall (x :: a) (z :: Apply f x). ShowSingApply' f x z) => ShowSingApply (f :: a ~> Type)

#if __GLASGOW_HASKELL__ >= 810
type ShowSingApply' :: forall a. forall (f :: a ~> Type) (x :: a) -> Apply f x -> Constraint
#endif
class    Show (Sing z) => ShowSingApply' (f :: a ~> Type) (x :: a) (z :: Apply f x)
instance Show (Sing z) => ShowSingApply' (f :: a ~> Type) (x :: a) (z :: Apply f x)
#endif
#endif
