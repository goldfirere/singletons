-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains basic functionality for deriving your own singletons
-- via Template Haskell. Note that this module does not define any singled
-- definitions on its own. For a version of this module that comes pre-equipped
-- with several singled definitions based on the "Prelude", see
-- @Data.Singletons.Base.TH@ from the @singletons-base@ library.
--
----------------------------------------------------------------------------

module Data.Singletons.TH (
  -- * Primary Template Haskell generation functions
  singletons, singletonsOnly, genSingletons,
  promote, promoteOnly, genDefunSymbols, genPromotions,

  -- ** Functions to generate equality instances
  promoteEqInstances, promoteEqInstance,
  singEqInstances, singEqInstance,
  singDecideInstances, singDecideInstance,

  -- ** Functions to generate 'Ord' instances
  promoteOrdInstances, promoteOrdInstance,
  singOrdInstances, singOrdInstance,

  -- ** Functions to generate 'Bounded' instances
  promoteBoundedInstances, promoteBoundedInstance,
  singBoundedInstances, singBoundedInstance,

  -- ** Functions to generate 'Enum' instances
  promoteEnumInstances, promoteEnumInstance,
  singEnumInstances, singEnumInstance,

  -- ** Functions to generate 'Show' instances
  promoteShowInstances, promoteShowInstance,
  singShowInstances, singShowInstance,
  showSingInstances, showSingInstance,

  -- ** Utility functions
  singITyConInstances, singITyConInstance,
  cases, sCases,

  -- * Basic singleton definitions
  module Data.Singletons,

  -- * Auxiliary definitions
  SDecide(..), (:~:)(..), Void, Refuted, Decision(..),

  SuppressUnusedWarnings(..)

 ) where

import Control.Arrow ( first )
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH.Options
import Data.Singletons.TH.Promote
import Data.Singletons.TH.Single
import Data.Singletons.TH.SuppressUnusedWarnings
import Data.Singletons.TH.Util
import Language.Haskell.TH
import Language.Haskell.TH.Desugar

-- | The function 'cases' generates a case expression where each right-hand side
-- is identical. This may be useful if the type-checker requires knowledge of which
-- constructor is used to satisfy equality or type-class constraints, but where
-- each constructor is treated the same.
cases :: DsMonad q
      => Name        -- ^ The head of the type of the scrutinee. (Like @''Maybe@ or @''Bool@.)
      -> q Exp       -- ^ The scrutinee, in a Template Haskell quote
      -> q Exp       -- ^ The body, in a Template Haskell quote
      -> q Exp
cases tyName expq bodyq = do
  dinfo <- dsReify tyName
  case dinfo of
    Just (DTyConI (DDataD _ _ _ _ _ ctors _) _) ->
      expToTH <$> buildCases (map extractNameArgs ctors) expq bodyq
    Just _ ->
      fail $ "Using <<cases>> with something other than a type constructor: "
              ++ (show tyName)
    _ -> fail $ "Cannot find " ++ show tyName

-- | The function 'sCases' generates a case expression where each right-hand side
-- is identical. This may be useful if the type-checker requires knowledge of which
-- constructor is used to satisfy equality or type-class constraints, but where
-- each constructor is treated the same. For 'sCases', unlike 'cases', the
-- scrutinee is a singleton. But make sure to pass in the name of the /original/
-- datatype, preferring @''Maybe@ over @''SMaybe@.
sCases :: OptionsMonad q
       => Name        -- ^ The head of the type the scrutinee's type is based on.
                      -- (Like @''Maybe@ or @''Bool@.)
       -> q Exp       -- ^ The scrutinee, in a Template Haskell quote
       -> q Exp       -- ^ The body, in a Template Haskell quote
       -> q Exp
sCases tyName expq bodyq = do
  opts  <- getOptions
  dinfo <- dsReify tyName
  case dinfo of
    Just (DTyConI (DDataD _ _ _ _ _ ctors _) _) ->
      let ctor_stuff = map (first (singledDataConName opts) . extractNameArgs) ctors in
      expToTH <$> buildCases ctor_stuff expq bodyq
    Just _ ->
      fail $ "Using <<cases>> with something other than a type constructor: "
              ++ (show tyName)
    _ -> fail $ "Cannot find " ++ show tyName

buildCases :: DsMonad m
           => [(Name, Int)]
           -> m Exp  -- scrutinee
           -> m Exp  -- body
           -> m DExp
buildCases ctor_infos expq bodyq =
  DCaseE <$> (dsExp =<< expq) <*>
             mapM (\con -> DMatch (conToPat con) <$> (dsExp =<< bodyq)) ctor_infos
  where
    conToPat :: (Name, Int) -> DPat
    conToPat (name, num_fields) =
      DConP name (replicate num_fields DWildP)
