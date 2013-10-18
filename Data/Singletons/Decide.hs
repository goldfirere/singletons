{- Data/Singletons/Decide.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Defines the SDecide, allowing for decidable equality over singletons.
-}

module Data.Singletons.Decide (
  SDecide(..),
  (:~:)(..), Void, Refuted, Decision(..)
  ) where

import Data.Singletons.Types
import Data.Singletons.Core

