-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Infer
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Infers constraints for a `deriving` class
--
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Data.Singletons.Deriving.Infer ( inferConstraints ) where

import Language.Haskell.TH.Desugar
import Data.Singletons.Util
import Data.List
import Data.Generics.Twins

inferConstraints :: DPred -> [DCon] -> DCxt
inferConstraints pr = nubBy geq . concatMap infer_ct
  where
#if MIN_VERSION_th_desugar(1,6,0)
    infer_ct (DCon _ _ _ fields _) = map (pr `DAppPr`) (tysOfConFields fields)
#else
    infer_ct (DCon _ _ _ fields) = map (pr `DAppPr`) (tysOfConFields fields)
#endif
