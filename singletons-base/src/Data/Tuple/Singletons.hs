{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple.Singletons
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines functions and datatypes relating to the singleton for tuples,
-- including singled versions of all the definitions in @Data.Tuple@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Tuple@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

module Data.Tuple.Singletons (
  -- * Singleton definitions
  -- | See 'Sing' for more info.

  Sing, STuple0(..), STuple2(..), STuple3(..),
  STuple4(..), STuple5(..), STuple6(..), STuple7(..),

  -- * Singletons from @Data.Tuple@
  Fst, sFst, Snd, sSnd, Curry, sCurry, Uncurry, sUncurry, Swap, sSwap,

  -- * Defunctionalization symbols
  Tuple0Sym0,
  Tuple2Sym0, Tuple2Sym1, Tuple2Sym2,
  Tuple3Sym0, Tuple3Sym1, Tuple3Sym2, Tuple3Sym3,
  Tuple4Sym0, Tuple4Sym1, Tuple4Sym2, Tuple4Sym3, Tuple4Sym4,
  Tuple5Sym0, Tuple5Sym1, Tuple5Sym2, Tuple5Sym3, Tuple5Sym4, Tuple5Sym5,
  Tuple6Sym0, Tuple6Sym1, Tuple6Sym2, Tuple6Sym3, Tuple6Sym4, Tuple6Sym5, Tuple6Sym6,
  Tuple7Sym0, Tuple7Sym1, Tuple7Sym2, Tuple7Sym3, Tuple7Sym4, Tuple7Sym5, Tuple7Sym6, Tuple7Sym7,

  FstSym0, FstSym1, SndSym0, SndSym1,
  CurrySym0, CurrySym1, CurrySym2, CurrySym3,
  UncurrySym0, UncurrySym1, UncurrySym2,
  SwapSym0, SwapSym1
  ) where

import Data.Singletons.Base.Instances
import Data.Singletons.TH

$(singletonsOnly [d|
  -- -| Extract the first component of a pair.
  fst                     :: (a,b) -> a
  fst (x,_)               =  x

  -- -| Extract the second component of a pair.
  snd                     :: (a,b) -> b
  snd (_,y)               =  y

  -- -| 'curry' converts an uncurried function to a curried function.
  curry                   :: ((a, b) -> c) -> a -> b -> c
  curry f x y             =  f (x, y)

  -- -| 'uncurry' converts a curried function to a function on pairs.
  uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
  uncurry f p             =  f (fst p) (snd p)

  -- -| Swap the components of a pair.
  swap                    :: (a,b) -> (b,a)
  swap (a,b)              = (b,a)
  |])
