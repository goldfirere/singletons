{- Data/Singletons/List.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This module exports functions for manipulating singleton lists. The actual definition
of the singleton list type is in Data.Singletons, to avoid orphan instances.
-}

{-# LANGUAGE CPP, TypeOperators, DataKinds, PolyKinds, TypeFamilies #-}

module Data.Singletons.List where

#if __GLASGOW_HASKELL__ >= 707

type family Head a where
  Head (h ': t) = h
type family Tail a where
  Tail (h ': t) = t

#else
    
-- operate on type-level lists
type family Head (a :: [k]) :: k
type instance Head (h ': t) = h

type family Tail (a :: [k]) :: [k]
type instance Tail (h ': t) = t

#endif