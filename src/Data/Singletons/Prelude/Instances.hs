{- Data/Singletons/Instances.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This (internal) module contains the main class definitions for singletons,
re-exported from various places.

-}

{-# LANGUAGE RankNTypes, TypeInType, GADTs, TypeFamilies, EmptyCase,
             FlexibleContexts, TemplateHaskell, ScopedTypeVariables,
             UndecidableInstances, TypeOperators, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Singletons.Prelude.Instances where

import Data.Singletons.Single
import Data.Singletons.Util

-- some useful singletons
$(genSingletons basicTypes)
$(singDecideInstances basicTypes)

-- basic definitions we need right away

$(singletonsOnly [d|
  foldl        :: forall a b. (b -> a -> b) -> b -> [a] -> b
  foldl f z0 xs0 = lgo z0 xs0
               where
                 lgo :: b -> [a] -> b
                 lgo z []     =  z
                 lgo z (x:xs) = lgo (f z x) xs
  |])
