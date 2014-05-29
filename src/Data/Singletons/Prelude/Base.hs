{-# LANGUAGE TemplateHaskell, KindSignatures, PolyKinds, TypeOperators,
             DataKinds, ScopedTypeVariables, TypeFamilies, GADTs,
             UndecidableInstances, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Prelude.Base
-- Copyright   :  (C) 2014 Jan Stolarek
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Jan Stolarek (jan.stolarek@p.lodz.pl)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements singletonized versions of functions from @GHC.Base@ module.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Tuple@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Prelude.Base (
  -- * Basic functions
  Foldr, sFoldr, Map, sMap, (:++), (%:++), Otherwise, sOtherwise,
  Id, sId, Const, sConst, (:.), (%:.), type ($), type ($!), (%$), (%$!),
  Flip, sFlip, AsTypeOf, sAsTypeOf,
  Seq, sSeq,

  -- * Defunctionalization symbols
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  MapSym0, MapSym1, MapSym2,
  (:++$), (:++$$),
  OtherwiseSym0,
  IdSym0, IdSym1,
  ConstSym0, ConstSym1, ConstSym2,
  (:.$), (:.$$), (:.$$$),
  type ($$), type ($$$), type ($$$$),
  type ($!$), type ($!$$), type ($!$$$),
  FlipSym0, FlipSym1, FlipSym2,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2,
  SeqSym0, SeqSym1, SeqSym2
  ) where

import Data.Singletons.Prelude.Instances
import Data.Singletons.Single
import Data.Singletons
import Data.Singletons.Prelude.Bool

-- Promoted and singletonized versions of "otherwise" are imported and
-- re-exported from Data.Singletons.Prelude.Bool. This is done to avoid cyclic
-- module dependencies.

$(singletonsOnly [d|
  foldr                   :: (a -> b -> b) -> b -> [a] -> b
  foldr k z = go
            where
              go []     = z
              go (y:ys) = y `k` go ys

  map                     :: (a -> b) -> [a] -> [b]
  map _ []                = []
  map f (x:xs)            = f x : map f xs

  (++)                    :: [a] -> [a] -> [a]
  (++) []     ys          = ys
  (++) (x:xs) ys          = x : xs ++ ys

  id                      :: a -> a
  id x                    =  x

  const                   :: a -> b -> a
  const x _               =  x

  (.)    :: (b -> c) -> (a -> b) -> a -> c
  (.) f g = \x -> f (g x)

  flip                    :: (a -> b -> c) -> b -> a -> c
  flip f x y              =  f y x

  asTypeOf                :: a -> a -> a
  asTypeOf                =  const

  -- This is not part of GHC.Base, but we need to emulate seq and this is a good
  -- place to do it.
  seq :: a -> b -> b
  seq _ x = x
 |])

-- ($) is a special case, because its kind-inference data constructors
-- clash with (:). See #29.
type family (f :: TyFun a b -> *) $ (x :: a) :: b
type instance f $ x = f @@ x

data ($$) :: TyFun (TyFun a b -> *) (TyFun a b -> *) -> *
type instance Apply ($$) arg = ($$$) arg

data ($$$) :: (TyFun a b -> *) -> TyFun a b -> *
type instance Apply (($$$) f) arg = ($$$$) f arg

type ($$$$) a b = ($) a b

(%$) :: forall (f :: TyFun a b -> *) (x :: a).
        Sing f -> Sing x -> Sing (($$) @@ f @@ x)
f %$ x = applySing f x

type family (f :: TyFun a b -> *) $! (x :: a) :: b
type instance f $! x = f @@ x

data ($!$) :: TyFun (TyFun a b -> *) (TyFun a b -> *) -> *
type instance Apply ($!$) arg = ($!$$) arg

data ($!$$) :: (TyFun a b -> *) -> TyFun a b -> *
type instance Apply (($!$$) f) arg = ($!$$$) f arg

type ($!$$$) a b = ($!) a b

(%$!) :: forall (f :: TyFun a b -> *) (x :: a).
        Sing f -> Sing x -> Sing (($!$) @@ f @@ x)
f %$! x = applySing f x
