{- Data/Singletons/Tuple.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Defines functions and datatypes relating to the singleton for Tuples.
-}

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, PolyKinds,
             RankNTypes, TypeFamilies, GADTs #-}

module Data.Singletons.Tuple (
  STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7,
  Sing(STuple0, STuple2, STuple3, STuple4, STuple5, STuple6, STuple7),
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