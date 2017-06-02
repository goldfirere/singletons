-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Ord
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Ord instances
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Ord ( mkOrdInstance ) where

import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Language.Haskell.TH.Syntax
import Data.Singletons.Deriving.Infer
import Data.Singletons.Syntax

-- | Make a *non-singleton* Ord instance
mkOrdInstance :: Quasi q => DType -> [DCon] -> q UInstDecl
mkOrdInstance ty cons = do
  let constraints = inferConstraints (DConPr ordName) cons
  -- This case also handles empty datatypes, since we can just create
  -- one clause with wildcard matches and zero fields to fold over
  compare_eq_clauses <- mapM mk_equal_clause $ if null cons
                                                  then [Nothing]
                                                  else map Just cons
  let compare_noneq_clauses = map (uncurry mk_nonequal_clause)
                                  [ (con1, con2)
                                  | con1 <- zip cons [1..]
                                  , con2 <- zip cons [1..]
                                  , extractName (fst con1) /=
                                    extractName (fst con2) ]
  return (InstDecl { id_cxt = constraints
                   , id_name = ordName
                   , id_arg_tys = [ty]
                   , id_meths = [( compareName
                                 , UFunction (compare_eq_clauses ++
                                              compare_noneq_clauses) )] })

mk_equal_clause :: Quasi q => Maybe DCon -- Nothing if it's an empty datatype,
                                         -- Just a constructor name otherwise
                           -> q DClause
mk_equal_clause mb_con = do
  let tys = maybe [] (snd . extractNameTypes) mb_con
  a_names <- mapM (const $ newUniqueName "a") tys
  b_names <- mapM (const $ newUniqueName "b") tys
  let mk_pat names = maybe DWildPa
                           (\con -> DConPa (extractName con) (map DVarPa names))
                           mb_con
      pat1 = mk_pat a_names
      pat2 = mk_pat b_names
  return $ DClause [pat1, pat2] (DVarE foldlName `DAppE`
                                 DVarE thenCmpName `DAppE`
                                 DConE cmpEQName `DAppE`
                                 mkListE (zipWith
                                          (\a b -> DVarE compareName `DAppE` DVarE a
                                                                     `DAppE` DVarE b)
                                          a_names b_names))

mk_nonequal_clause :: (DCon, Int) -> (DCon, Int) -> DClause
mk_nonequal_clause (DCon _tvbs1 _cxt1 name1 fields1 _rty1, n1)
                   (DCon _tvbs2 _cxt2 name2 fields2 _rty2, n2) =
  DClause [pat1, pat2] (case n1 `compare` n2 of
                          LT -> DConE cmpLTName
                          EQ -> DConE cmpEQName
                          GT -> DConE cmpGTName)
  where
    pat1 = DConPa name1 (map (const DWildPa) (tysOfConFields fields1))
    pat2 = DConPa name2 (map (const DWildPa) (tysOfConFields fields2))
