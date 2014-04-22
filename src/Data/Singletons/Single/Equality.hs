{- Data/Singletons/Single/Equality.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Defines functions to generate SEq and SDecide instances.
-}

module Data.Singletons.Single.Equality where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Sweeten
import Data.Singletons.Util
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Control.Monad

