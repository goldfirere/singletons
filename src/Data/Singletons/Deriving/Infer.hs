-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Infer
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Infers constraints for a `deriving` class
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Infer ( inferConstraints, inferConstraintsDef ) where

import Language.Haskell.TH.Desugar
import Data.Singletons.Util
import Data.List
import Data.Maybe (fromMaybe)
import Data.Generics.Twins

inferConstraints :: DPred -> [DCon] -> DCxt
inferConstraints pr = nubBy geq . concatMap infer_ct
  where
    infer_ct (DCon _ _ _ fields _) = map (pr `DAppPr`) (tysOfConFields fields)

inferConstraintsDef :: Maybe DCxt -> DPred -> [DCon] -> DCxt
inferConstraintsDef mb_ctxt pr cons = fromMaybe (inferConstraints pr cons) mb_ctxt
