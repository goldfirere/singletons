-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Eq
-- Copyright   :  (C) 2020 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Eq instances
--
----------------------------------------------------------------------------
module Data.Singletons.Deriving.Eq (mkEqInstance) where

import Control.Monad
import Data.Singletons.Deriving.Infer
import Data.Singletons.Deriving.Util
import Data.Singletons.Names
import Data.Singletons.Syntax
import Data.Singletons.Util
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax

mkEqInstance :: DsMonad q => DerivDesc q
mkEqInstance mb_ctxt ty (DataDecl _ _ cons) = do
  let con_pairs = [ (c1, c2) | c1 <- cons, c2 <- cons ]
  constraints <- inferConstraintsDef mb_ctxt (DConT eqName) ty cons
  clauses <- if null cons
             then pure [DClause [DWildP, DWildP] (DConE trueName)]
             else traverse mkEqClause con_pairs
  pure (InstDecl { id_cxt = constraints
                 , id_name = eqName
                 , id_arg_tys = [ty]
                 , id_sigs  = mempty
                 , id_meths = [(equalsName, UFunction clauses)] })

mkEqClause :: Quasi q => (DCon, DCon) -> q DClause
mkEqClause (c1, c2)
  | lname == rname = do
      lnames <- replicateM lNumArgs (newUniqueName "a")
      rnames <- replicateM lNumArgs (newUniqueName "b")
      let lpats = map DVarP lnames
          rpats = map DVarP rnames
          lvars = map DVarE lnames
          rvars = map DVarE rnames
      pure $ DClause
        [DConP lname lpats, DConP rname rpats]
        (andExp (zipWith (\l r -> foldExp (DVarE equalsName) [l, r])
                         lvars rvars))
  | otherwise =
      pure $ DClause
        [DConP lname (replicate lNumArgs DWildP),
         DConP rname (replicate rNumArgs DWildP)]
        (DConE falseName)
  where
    andExp :: [DExp] -> DExp
    andExp []    = DConE trueName
    andExp [one] = one
    andExp (h:t) = DVarE andName `DAppE` h `DAppE` andExp t

    (lname, lNumArgs) = extractNameArgs c1
    (rname, rNumArgs) = extractNameArgs c2
