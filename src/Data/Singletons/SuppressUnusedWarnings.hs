-- Data/Singletons/Hidden.hs
--
-- (c) Richard Eisenberg 2014
-- eir@cis.upenn.edu
--
-- This declares user-oriented exports that are actually meant to be hidden
-- from the user. Why would anyone ever want this? Because what is below
-- is dirty, and no one wants to see it.

{-# LANGUAGE PolyKinds #-}

module Data.Singletons.SuppressUnusedWarnings where

import Data.Proxy

-- | This class (which users should never see) is to be instantiated in order
-- to use an otherwise-unused data constructor, such as the "kind-inference"
-- data constructor for defunctionalization symbols.
class SuppressUnusedWarnings (t :: k) where
  suppressUnusedWarnings :: Proxy t -> ()
