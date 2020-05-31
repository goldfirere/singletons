-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Bounded
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Bounded instances
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Bounded where

import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Data.Singletons.Syntax
import Data.Singletons.Deriving.Infer
import Data.Singletons.Deriving.Util
import Control.Monad

-- monadic only for failure and parallelism with other functions
-- that make instances
mkBoundedInstance :: DsMonad q => DerivDesc q
mkBoundedInstance mb_ctxt ty (DataDecl _ _ cons) = do
  -- We can derive instance of Bounded if datatype is an enumeration (all
  -- constructors must be nullary) or has only one constructor. See Section 11
  -- of Haskell 2010 Language Report.
  -- Note that order of conditions below is important.
  when (null cons
       || (any (\(DCon _ _ _ f _) -> not . null . tysOfConFields $ f) cons
            && (not . null . tail $ cons))) $
       fail ("Can't derive Bounded instance for "
             ++ pprint (typeToTH ty) ++ ".")
  -- at this point we know that either we have a datatype that has only one
  -- constructor or a datatype where each constructor is nullary
  let (DCon _ _ minName fields _) = head cons
      (DCon _ _ maxName _ _)      = last cons
      fieldsCount   = length $ tysOfConFields fields
      (minRHS, maxRHS) = case fieldsCount of
        0 -> (DConE minName, DConE maxName)
        _ ->
          let minEqnRHS = foldExp (DConE minName)
                                  (replicate fieldsCount (DVarE minBoundName))
              maxEqnRHS = foldExp (DConE maxName)
                                  (replicate fieldsCount (DVarE maxBoundName))
          in (minEqnRHS, maxEqnRHS)

      mk_rhs rhs = UFunction [DClause [] rhs]
  constraints <- inferConstraintsDef mb_ctxt (DConT boundedName) ty cons
  return $ InstDecl { id_cxt = constraints
                    , id_name = boundedName
                    , id_arg_tys = [ty]
                    , id_sigs  = mempty
                    , id_meths = [ (minBoundName, mk_rhs minRHS)
                                 , (maxBoundName, mk_rhs maxRHS) ] }
