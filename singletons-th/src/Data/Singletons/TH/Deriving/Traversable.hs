{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Deriving.Traversable
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Traversable instances
--
----------------------------------------------------------------------------

module Data.Singletons.TH.Deriving.Traversable where

import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Syntax
import Language.Haskell.TH.Desugar

mkTraversableInstance :: forall q. DsMonad q => DerivDesc q
mkTraversableInstance mb_ctxt ty dd@(DataDecl _ _ cons) = do
  functorLikeValidityChecks False dd
  f <- newUniqueName "_f"
  let ft_trav :: FFoldType (q DExp)
      ft_trav = FT { ft_triv = pure $ DVarE pureName
                     -- traverse f = pure x
                   , ft_var = pure $ DVarE f
                     -- traverse f = f x
                   , ft_ty_app = \_ g -> DAppE (DVarE traverseName) <$> g
                     -- traverse f = traverse g
                   , ft_forall = \_ g -> g
                   , ft_bad_app = error "in other argument in ft_trav"
                   }

      -- Con a1 a2 ... -> Con <$> g1 a1 <*> g2 a2 <*> ...
      clause_for_con :: [DPat] -> DCon -> [DExp] -> q DClause
      clause_for_con = mkSimpleConClause $ \con_name -> mkApCon (DConE con_name)
        where
          -- ((Con <$> x1) <*> x2) <*> ...
          mkApCon :: DExp -> [DExp] -> DExp
          mkApCon con []  = DVarE pureName `DAppE` con
          mkApCon con [x] = DVarE fmapName `DAppE` con `DAppE` x
          mkApCon con (x1:x2:xs) =
              foldl appAp (DVarE liftA2Name `DAppE` con `DAppE` x1 `DAppE` x2) xs
            where appAp x y = DVarE apName `DAppE` x `DAppE` y

      mk_trav_clause :: DCon -> q DClause
      mk_trav_clause con = do
        parts <- foldDataConArgs ft_trav con
        clause_for_con [DVarP f] con =<< sequence parts

      mk_trav :: q [DClause]
      mk_trav = case cons of
                  [] -> do v <- newUniqueName "v"
                           pure [DClause [DWildP, DVarP v]
                                         (DVarE pureName `DAppE` DCaseE (DVarE v) [])]
                  _  -> traverse mk_trav_clause cons

  trav_clauses <- mk_trav
  constraints <- inferConstraintsDef mb_ctxt (DConT traversableName) ty cons
  return $ InstDecl { id_cxt = constraints
                    , id_name = traversableName
                    , id_arg_tys = [ty]
                    , id_sigs  = mempty
                    , id_meths = [ (traverseName, UFunction trav_clauses) ] }
