-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promotion.Prelude.Function
-- Copyright   :  (C) 2016 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines promoted functions from @Data.Function@.
--
-- Because many of these definitions are produced by Template Haskell,
-- it is not possible to create proper Haddock documentation. Please look
-- up the corresponding operation in @Data.Function@. Also, please excuse
-- the apparent repeated variable names. This is due to an interaction
-- between Template Haskell and Haddock.
--
----------------------------------------------------------------------------

{-# LANGUAGE ExplicitNamespaces #-}

module Data.Promotion.Prelude.Function (
    -- * "Prelude" re-exports
    Id, Const, (:.), Flip, type ($)
    -- * Other combinators
  , (:&), On

    -- * Defunctionalization symbols
  , IdSym0, IdSym1
  , ConstSym0, ConstSym1, ConstSym2
  , (:.$), (:.$$), (:.$$$), (:.$$$$)
  , FlipSym0, FlipSym1, FlipSym2, FlipSym3
  , type ($$), type ($$$), type ($$$$)
  , (:&$), (:&$$), (:&$$$)
  , OnSym0, OnSym1, OnSym2, OnSym3, OnSym4
  ) where

import Data.Singletons.Prelude.Function
