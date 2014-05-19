{- Data/Singletons/Instances.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This (internal) module contains the main class definitions for singletons,
re-exported from various places.

-}

{-# LANGUAGE RankNTypes, DataKinds, PolyKinds, GADTs, TypeFamilies,
             FlexibleContexts, TemplateHaskell, ScopedTypeVariables,
             UndecidableInstances, TypeOperators, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Singletons.Prelude.Instances where

import Data.Singletons.Single
import Data.Singletons.Util

-- some useful singletons
$(genSingletons basicTypes)
$(singDecideInstances basicTypes)
