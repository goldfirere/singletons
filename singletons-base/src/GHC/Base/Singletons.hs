{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base.Singletons
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

module GHC.Base.Singletons (
  -- * Basic functions
  Foldr, sFoldr, Map, sMap, type (++), (%++), Otherwise, sOtherwise,
  Id, sId, Const, sConst, type (.), (%.), type ($), type ($!), (%$), (%$!),
  Until, sUntil, Flip, sFlip, AsTypeOf, sAsTypeOf,
  Seq, sSeq,

  -- * Defunctionalization symbols
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  MapSym0, MapSym1, MapSym2,
  type (++@#@$), type (++@#@$$), type (++@#@$$$),
  OtherwiseSym0,
  IdSym0, IdSym1,
  ConstSym0, ConstSym1, ConstSym2,
  type (.@#@$),  type (.@#@$$),  type (.@#@$$$), type (.@#@$$$$),
  type ($@#@$),  type ($@#@$$),  type ($@#@$$$),
  type ($!@#@$), type ($!@#@$$), type ($!@#@$$$),
  UntilSym0, UntilSym1, UntilSym2, UntilSym3,
  FlipSym0, FlipSym1, FlipSym2, FlipSym3,
  AsTypeOfSym0, AsTypeOfSym1, AsTypeOfSym2,
  SeqSym0, SeqSym1, SeqSym2
  ) where

import Data.Bool.Singletons
import Data.Singletons.Base.Instances
import Data.Singletons.TH

-- Promoted and singletonized versions of "otherwise" are imported and
-- re-exported from Data.Bool.Singletons. This is done to avoid cyclic
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
  infixr 5 ++

  id                      :: a -> a
  id x                    =  x

  const                   :: a -> b -> a
  const x _               =  x

  (.)    :: (b -> c) -> (a -> b) -> a -> c
  (.) f g = \x -> f (g x)
  infixr 9 .

  flip                    :: (a -> b -> c) -> b -> a -> c
  flip f x y              =  f y x

  asTypeOf                :: a -> a -> a
  asTypeOf                =  const

  ($)                     :: (a -> b) -> a -> b
  f $ x                   =  f x
  infixr 0 $

  ($!)                    :: (a -> b) -> a -> b
  f $! x                  = let {-!-}vx = x in f vx
  infixr 0 $!

  until                   :: (a -> Bool) -> (a -> a) -> a -> a
  until p f = go
    where
      -- Does not singletonize due to overlapping patterns.
      {-
      go x | p x          = x
           | otherwise    = go (f x)
      -}
      go x = if p x then x else go (f x)

  -- This is not part of GHC.Base, but we need to emulate seq and this is a good
  -- place to do it.
  seq :: a -> b -> b
  seq _ x = x
  infixr 0 `seq`
 |])
