{- Data/Singletons/List.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This module exports functions for manipulating singleton lists. The actual definition
of the singleton list type is in Data.Singletons, to avoid orphan instances.
-}

{-# LANGUAGE CPP, TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Data.Singletons.List (
  Head, Tail,
  (:++), (%:++)
  ) where

import Data.Singletons

$(singletons [d|
  (++) :: [a] -> [a] -> [a]
  [] ++ a = a
  (h:t) ++ a = h:(t ++ a)
  |])
