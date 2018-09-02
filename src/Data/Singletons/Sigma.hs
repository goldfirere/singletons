{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
    ( Sigma(..), Σ
    , projSigma1, projSigma2
    , currySigma, uncurrySigma
    , mapSigma, zipSigma

      -- * Defunctionalization symbols
    , ΣSym0, ΣSym1, ΣSym2
    ) where

import Data.Kind (Type)
import Data.Singletons.Internal
import Data.Singletons.Promote

-- | A dependent pair.
data Sigma (s :: Type) :: (s ~> Type) -> Type where
  (:&:) :: forall s t fst. Sing (fst :: s) -> t @@ fst -> Sigma s t
infixr 4 :&:

-- | Unicode shorthand for 'Sigma'.
type Σ (s :: Type) (t :: s ~> Type) = Sigma s t

-- | Project the first element out of a dependent pair.
projSigma1 :: forall s t. SingKind s => Sigma s t -> Demote s
projSigma1 (a :&: _) = fromSing a

-- | Project the second element out of a dependent pair.
--
-- In an ideal setting, the type of 'projSigma2' would be closer to:
--
-- @
-- 'projSigma2' :: 'Sing' (sig :: 'Sigma' s t) -> t @@ ProjSigma1 sig
-- @
--
-- But promoting 'projSigma1' to a type family is not a simple task. Instead,
-- we do the next-best thing, which is to use Church-style elimination.
projSigma2 :: forall s t r. (forall (fst :: s). t @@ fst -> r) -> Sigma s t -> r
projSigma2 f ((_ :: Sing (fst :: s)) :&: b) = f @fst b

-- | One half to witnessing an isomorphism. Convert a uncurried
-- function on 'Sigma' to a curried one.
-- 
-- Alternative type
-- 
-- @
-- -- (~@>) :: (k ~> Type) -> (k ~> Type) -> Type
-- type f ~@> g = (forall xx. Sing (xx::k) -> f@@xx -> g@@xx)
-- 
-- 'currySigma' :: (Sigma k f -> b) -> (f ~@> 'ConstSym1' b)
-- @
--
-- Such that the following holds:
-- 
-- @
-- id1 :: ('Sigma' k f -> b) -> ('Sigma' k f -> b)
-- id1 f = 'uncurrySigma' ('currySigma' f)
--
-- id2 :: forall f b. (f ~@> ConstSym1 b) -> (f ~@> ConstSym1 b)
-- id2 f = 'currySigma' @_ @_ @f ('uncurrySigma' @_ @_ @f f)
-- @
-- 
currySigma
  :: (Sigma k f -> b)
  -> (forall (a::k). Sing a -> f@@a -> b)
currySigma f sing fa = f (sing :&: fa)

-- | Elimination of 'Sigma'. Second half of isomorphism.
-- 
-- Alternative type
-- 
-- @
-- 'uncurrySigma' :: (f ~@> 'ConstSym1' b) -> ('Sigma' k f -> b)
-- @
-- 
uncurrySigma
  :: (forall (a::k). Sing a -> f@@a -> b)
  -> (Sigma k f -> b)
uncurrySigma elim ((fst :: Sing fst) :&: f_fst) = elim @fst fst f_fst

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

$(genDefunSymbols [''Σ])
