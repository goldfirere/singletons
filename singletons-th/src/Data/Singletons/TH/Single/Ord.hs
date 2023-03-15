-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Single.Ord
-- Copyright   :  (C) 2023 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines a function to generate boilerplate Ord instances for singleton
-- types.
--
-----------------------------------------------------------------------------

module Data.Singletons.TH.Single.Ord (mkOrdInstanceForSingleton) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Promote.Type

-- Make a boilerplate Ord instance for a singleton type, e.g.,
--
-- @
-- instance Ord (SExample (z :: Example a)) where
--   compare _ _ = EQ
-- @
mkOrdInstanceForSingleton :: OptionsMonad q
                          => DType
                          -> Name
                          -- ^ The name of the data type
                          -> q DDec
mkOrdInstanceForSingleton data_ty data_name = do
  opts <- getOptions
  z <- qNewName "z"
  data_ki <- promoteType data_ty
  let sdata_name = singledDataTypeName opts data_name
  pure $ DInstanceD Nothing Nothing []
           (DAppT (DConT ordName) (DConT sdata_name `DAppT` DSigT (DVarT z) data_ki))
           [DLetDec $
            DFunD compareName
                  [DClause [DWildP, DWildP] (DConE cmpEQName)]]
