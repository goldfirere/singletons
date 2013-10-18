{- Data/Singletons/List.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This module exports functions for manipulating singleton lists. The actual definition
of the singleton list type is in Data.Singletons, to avoid orphan instances.
-}

{-# LANGUAGE CPP, TypeOperators, DataKinds, PolyKinds, TypeFamilies,
             TemplateHaskell, GADTs, UndecidableInstances #-}

module Data.Singletons.List (
  Sing(SNil, SCons), SList,
  Head, Tail, sHead, sTail,
  (:++), (%:++),
  Reverse, sReverse
  ) where

import Data.Singletons.Core
import Data.Singletons
import Data.Singletons.Singletons

$(singletonsOnly [d|
  (++) :: [a] -> [a] -> [a]
  [] ++ a = a
  (h:t) ++ a = h:(t ++ a)

  head :: [a] -> a
  head (a : _) = a
  head []      = error "Data.Singletons.List.head: empty list"

  tail :: [a] -> [a]
  tail (_ : t) = t
  tail []      = error "Data.Singletons.List.tail: empty list"

  reverse :: [a] -> [a]
  reverse list = reverse_aux [] list

  reverse_aux :: [a] -> [a] -> [a]
  reverse_aux acc []      = acc
  reverse_aux acc (h : t) = reverse_aux (h : acc) t
  |])


