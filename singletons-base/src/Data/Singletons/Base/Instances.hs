{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- Data/Singletons/Base/Instances.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This (internal) module contains the main class definitions for singletons-base,
re-exported from various places.

-}

module Data.Singletons.Base.Instances (
    module Data.Singletons.Base.Instances
  , Sing
  ) where

import Data.Singletons
import Data.Singletons.Base.Util
import Data.Singletons.TH

-- some useful singletons
$(genSingletons basicTypes)
$(singDecideInstances basicTypes)
$(showSingInstances basicTypes)

-- basic definitions we need right away

$(singletonsOnly [d|
  foldl        :: forall a b. (b -> a -> b) -> b -> [a] -> b
  foldl f z0 xs0 = lgo z0 xs0
               where
                 lgo :: b -> [a] -> b
                 lgo z []     =  z
                 lgo z (x:xs) = lgo (f z x) xs
  |])
