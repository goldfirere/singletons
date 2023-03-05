-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Deriving.Ord
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Ord instances
--
----------------------------------------------------------------------------

module Data.Singletons.TH.Deriving.Ord ( mkOrdInstance ) where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util

-- | Make a *non-singleton* Ord instance
mkOrdInstance :: DsMonad q => DerivDesc q
mkOrdInstance mb_ctxt ty (DataDecl _ _ _ cons) = do
  constraints <- inferConstraintsDef mb_ctxt (DConT ordName) ty cons
  compare_eq_clauses <- mapM mk_equal_clause cons
  let compare_noneq_clauses = map (uncurry mk_nonequal_clause)
                                  [ (con1, con2)
                                  | con1 <- zip cons [1..]
                                  , con2 <- zip cons [1..]
                                  , extractName (fst con1) /=
                                    extractName (fst con2) ]
      clauses | null cons = [mk_empty_clause]
              | otherwise = compare_eq_clauses ++ compare_noneq_clauses
  return (InstDecl { id_cxt = constraints
                   , id_name = ordName
                   , id_arg_tys = [ty]
                   , id_sigs  = mempty
                   , id_meths = [(compareName, UFunction clauses)] })

mk_equal_clause :: Quasi q => DCon -> q DClause
mk_equal_clause (DCon _tvbs _cxt name fields _rty) = do
  let tys = tysOfConFields fields
  a_names <- mapM (const $ newUniqueName "a") tys
  b_names <- mapM (const $ newUniqueName "b") tys
  let pat1 = DConP name [] (map DVarP a_names)
      pat2 = DConP name [] (map DVarP b_names)
  return $ DClause [pat1, pat2] (DVarE foldlName `DAppE`
                                 DVarE sappendName `DAppE`
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
    pat1 = DConP name1 [] (map (const DWildP) (tysOfConFields fields1))
    pat2 = DConP name2 [] (map (const DWildP) (tysOfConFields fields2))

-- A variant of mk_equal_clause tailored to empty datatypes
mk_empty_clause :: DClause
mk_empty_clause = DClause [DWildP, DWildP] (DConE cmpEQName)
