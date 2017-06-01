-- Data/Singletons/Hidden.hs
--
-- (c) Richard Eisenberg 2014
-- rae@cs.brynmawr.edu
--
-- This declares user-oriented exports that are actually meant to be hidden
-- from the user. Why would anyone ever want this? Because what is below
-- is dirty, and no one wants to see it.

{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

module Data.Singletons.SuppressUnusedWarnings where

-- | This class (which users should never see) is to be instantiated in order
-- to use an otherwise-unused data constructor, such as the "kind-inference"
-- data constructor for defunctionalization symbols.
class SuppressUnusedWarnings (t :: k) where
  suppressUnusedWarnings :: ()
