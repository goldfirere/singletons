{- Data/Singletons/Legacy.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file contains definitions to support legacy compilers, like GHC 7.6.3
-}

{-# LANGUAGE KindSignatures #-}

module Data.Singletons.Legacy where

-- now in Data.Proxy
data KProxy (a :: *) = KProxy
