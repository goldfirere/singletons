{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, PolyKinds,
             RankNTypes, TypeFamilies, GADTs, CPP #-}

#if __GLASGOW_HASKELL__ < 707
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Tuple
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for tuples,
-- including a singletons version of all the definitions in @Data.Tuple@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Tuple@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Singletons.Tuple (
  -- * Singleton definitions
  -- | See 'Data.Singletons.Prelude.Sing' for more info.
  Sing(STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7),
  STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,

  -- * Singletons from @Data.Tuple@
  Fst, sFst, Snd, sSnd, Curry, sCurry, Uncurry, sUncurry, Swap, sSwap
  ) where

import Data.Singletons.Core
import Data.Singletons.TH

$(singletonsOnly [d|
  -- | Extract the first component of a pair.
  fst                     :: (a,b) -> a
  fst (x,_)               =  x

  -- | Extract the second component of a pair.
  snd                     :: (a,b) -> b
  snd (_,y)               =  y

  -- | 'curry' converts an uncurried function to a curried function.
  curry                   :: ((a, b) -> c) -> a -> b -> c
  curry f x y             =  f (x, y)

  -- | 'uncurry' converts a curried function to a function on pairs.
  uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
  uncurry f p             =  f (fst p) (snd p)

  -- | Swap the components of a pair.
  swap                    :: (a,b) -> (b,a)
  swap (a,b)              = (b,a)
  |])