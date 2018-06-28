{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Functor
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Implements deriving of Functor instances
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Functor where

import Data.Singletons.Deriving.Infer
import Data.Singletons.Deriving.Util
import Data.Singletons.Names
import Data.Singletons.Syntax
import Data.Singletons.Util
import Language.Haskell.TH.Desugar

mkFunctorInstance :: forall q. DsMonad q => DerivDesc q
mkFunctorInstance mb_ctxt ty dd@(DataDecl _ _ cons) = do
  functorLikeValidityChecks False dd
  f <- newUniqueName "_f"
  z <- newUniqueName "_z"
  let ft_fmap :: FFoldType (q DExp)
      ft_fmap = FT { ft_triv = mkSimpleLam pure
                     -- fmap f = \x -> x
                   , ft_var = pure $ DVarE f
                     -- fmap f = f
                   , ft_ty_app = \_ g -> DAppE (DVarE fmapName) <$> g
                     -- fmap f = fmap g
                   , ft_forall = \_ g -> g
                   , ft_bad_app = error "in other argument in ft_fmap"
                   }

      ft_replace :: FFoldType (q Replacer)
      ft_replace = FT { ft_triv = fmap Nested    $ mkSimpleLam pure
                        -- (p <$) = \x -> x
                      , ft_var  = fmap Immediate $ mkSimpleLam $ \_ -> pure $ DVarE z
                        -- (p <$) = const p
                      , ft_ty_app = \_ gm -> do
                          g <- gm
                          case g of
                            Nested g'   -> pure . Nested $ DVarE fmapName    `DAppE` g'
                            Immediate _ -> pure . Nested $ DVarE replaceName `DAppE` DVarE z
                        -- (p <$) = fmap (p <$)
                      , ft_forall  = \_ g -> g
                      , ft_bad_app = error "in other argument in ft_replace"
                      }

      -- Con a1 a2 ... -> Con (f1 a1) (f2 a2) ...
      clause_for_con :: [DPat] -> DCon -> [DExp] -> q DClause
      clause_for_con = mkSimpleConClause $ \con_name ->
        foldExp (DConE con_name) -- Con x1 x2 ...

      mk_fmap_clause :: DCon -> q DClause
      mk_fmap_clause con = do
        parts <- foldDataConArgs ft_fmap con
        clause_for_con [DVarPa f] con =<< sequence parts

      mk_replace_clause :: DCon -> q DClause
      mk_replace_clause con = do
        parts <- foldDataConArgs ft_replace con
        clause_for_con [DVarPa z] con =<< traverse (fmap replace) parts

      mk_fmap :: q [DClause]
      mk_fmap = case cons of
                  [] -> do v <- newUniqueName "v"
                           pure [DClause [DWildPa, DVarPa v] (DCaseE (DVarE v) [])]
                  _  -> traverse mk_fmap_clause cons

      mk_replace :: q [DClause]
      mk_replace = case cons of
                     [] -> do v <- newUniqueName "v"
                              pure [DClause [DWildPa, DVarPa v] (DCaseE (DVarE v) [])]
                     _  -> traverse mk_replace_clause cons

  fmap_clauses    <- mk_fmap
  replace_clauses <- mk_replace
  constraints <- inferConstraintsDef mb_ctxt (DConPr functorName) ty cons
  return $ InstDecl { id_cxt = constraints
                    , id_name = functorName
                    , id_arg_tys = [ty]
                    , id_meths = [ (fmapName,    UFunction fmap_clauses)
                                 , (replaceName, UFunction replace_clauses)
                                 ] }

data Replacer = Immediate { replace :: DExp }
              | Nested    { replace :: DExp }
