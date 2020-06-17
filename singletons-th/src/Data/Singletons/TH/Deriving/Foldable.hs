{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Deriving.Foldable
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Foldable instances
--
----------------------------------------------------------------------------

module Data.Singletons.TH.Deriving.Foldable where

import Data.Singletons.TH.Deriving.Infer
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Syntax
import Language.Haskell.TH.Desugar

mkFoldableInstance :: forall q. DsMonad q => DerivDesc q
mkFoldableInstance mb_ctxt ty dd@(DataDecl _ _ cons) = do
  functorLikeValidityChecks False dd
  f <- newUniqueName "_f"
  z <- newUniqueName "_z"
  let ft_foldMap :: FFoldType (q DExp)
      ft_foldMap = FT { ft_triv = mkSimpleLam $ \_ -> pure $ DVarE memptyName
                        -- foldMap f = \x -> mempty
                      , ft_var = pure $ DVarE f
                        -- foldMap f = f
                      , ft_ty_app = \_ g -> DAppE (DVarE foldMapName) <$> g
                        -- foldMap f = foldMap g
                      , ft_forall  = \_ g -> g
                      , ft_bad_app = error "in other argument in ft_foldMap"
                      }

      ft_foldr :: FFoldType (q DExp)
      ft_foldr = FT { ft_triv = mkSimpleLam2 $ \_ z' -> pure z'
                      -- foldr f = \x z -> z
                    , ft_var  = pure $ DVarE f
                      -- foldr f = f
                    , ft_ty_app = \_ g -> do
                        gg <- g
                        mkSimpleLam2 $ \x z' -> pure $
                          DVarE foldrName `DAppE` gg `DAppE` z' `DAppE` x
                      -- foldr f = (\x z -> foldr g z x)
                    , ft_forall  = \_ g -> g
                    , ft_bad_app = error "in other argument in ft_foldr"
                    }

      clause_for_foldMap :: [DPat] -> DCon -> [DExp] -> q DClause
      clause_for_foldMap = mkSimpleConClause $ \_ -> mkFoldMap
        where
          -- mappend v1 (mappend v2 ..)
          mkFoldMap :: [DExp] -> DExp
          mkFoldMap [] = DVarE memptyName
          mkFoldMap xs = foldr1 (\x y -> DVarE mappendName `DAppE` x `DAppE` y) xs

      clause_for_foldr :: [DPat] -> DCon -> [DExp] -> q DClause
      clause_for_foldr = mkSimpleConClause $ \_ -> mkFoldr
        where
          -- g1 v1 (g2 v2 (.. z))
          mkFoldr :: [DExp] -> DExp
          mkFoldr = foldr DAppE (DVarE z)

      mk_foldMap_clause :: DCon -> q DClause
      mk_foldMap_clause con = do
        parts <- foldDataConArgs ft_foldMap con
        clause_for_foldMap [DVarP f] con =<< sequence parts

      mk_foldr_clause :: DCon -> q DClause
      mk_foldr_clause con = do
        parts <- foldDataConArgs ft_foldr con
        clause_for_foldr [DVarP f, DVarP z] con =<< sequence parts

      mk_foldMap :: q [DClause]
      mk_foldMap =
        case cons of
          [] -> pure [DClause [DWildP, DWildP] (DVarE memptyName)]
          _  -> traverse mk_foldMap_clause cons

      mk_foldr :: q [DClause]
      mk_foldr = traverse mk_foldr_clause cons

  foldMap_clauses <- mk_foldMap
  foldr_clauses   <- mk_foldr
  let meths = (foldMapName, UFunction foldMap_clauses)
              : case cons of
                  [] -> []
                  _  -> [(foldrName, UFunction foldr_clauses)]
  constraints <- inferConstraintsDef mb_ctxt (DConT foldableName) ty cons
  return $ InstDecl { id_cxt = constraints
                    , id_name = foldableName
                    , id_arg_tys = [ty]
                    , id_sigs  = mempty
                    , id_meths = meths }
