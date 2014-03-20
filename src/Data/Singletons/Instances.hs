{- Data/Singletons/Instances.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This (internal) module contains the main class definitions for singletons,
re-exported from various places.

-}

{-# LANGUAGE CPP, RankNTypes, DataKinds, PolyKinds, GADTs, TypeFamilies,
             FlexibleContexts, TemplateHaskell, ScopedTypeVariables,
             UndecidableInstances, TypeOperators, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ < 707
  -- optimizing instances of SDecide cause GHC to die (#8467)
{-# OPTIONS_GHC -O0 #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Singletons.Instances where

import Data.Singletons.Singletons
import Data.Singletons.Util

-- some useful singletons
$(genSingletons basicTypes)
$(singDecideInstances basicTypes)

