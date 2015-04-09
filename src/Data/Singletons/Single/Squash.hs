{- Data/Singletons/Single/Squash.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Squash top-level wildcard patterns.
-}

{-# LANGUAGE TemplateHaskell, CPP #-}

module Data.Singletons.Single.Squash where

import Prelude hiding ( exp )

import Language.Haskell.TH.Desugar
#if __GLASGOW_HASKELL__ < 710
-- We don't need thess imports for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
import Data.Monoid
#endif
import Control.Monad
import Data.Singletons.Util
import Data.Singletons.Names
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Language.Haskell.TH.Syntax
import Control.Arrow

squashWildcards :: DsMonad q => [DLetDec] -> q [DLetDec]
squashWildcards = freshenNames <=< mapM (go_let_dec <=< scLetDec)
  where
    go (DAppE e1 e2) = DAppE <$> go e1 <*> go e2
    go (DLamE vs e)  = DLamE vs <$> go e
    go (DCaseE scrut matches) = DCaseE <$> go scrut <*> go_matches matches
    go (DLetE let_decs e) = DLetE <$> mapM go_let_dec let_decs <*> go e
    go (DSigE e t) = DSigE <$> go e <*> pure t
    go e = return e

    go_let_dec (DFunD name [DClause pats exp]) = do
      exp' <- go exp
      return $ DFunD name [DClause pats exp']
    go_let_dec d@(DFunD {}) = error $ "Impossible scLetDec result: " ++ show d
    go_let_dec (DValD pat exp) = DValD pat <$> go exp
    go_let_dec d               = return d

    -- at this point, all top-level patterns are constructors (with variable args),
    -- literals, or wildcards
    go_matches = go_match [] []

    go_match _ matches [] = return (reverse matches)   -- no wildcard pattern
    go_match used matches (DMatch DWildPa rhs : _) = do
      rhs' <- go rhs
      new_matches <- mk_wild_matches used rhs'
      return (reverse matches ++ new_matches)
    go_match _ matches ms@(DMatch (DLitPa {}) _ : _) = do
      ms' <- mapM go_match_simply ms
      return (reverse matches ++ ms')
    go_match used matches (m@(DMatch (DConPa name _) _) : ms) = do
      m' <- go_match_simply m
      go_match (name:used) (m':matches) ms
    go_match _ _ ms =
      error $ "Internal error in singletons: Unexpected pattern in go_match: " ++ show ms

    -- just deal with the RHS
    go_match_simply (DMatch pat rhs) = DMatch pat <$> go rhs

    mk_wild_matches [] rhs = return [DMatch DWildPa rhs]
      -- no constructors used. literal match? silly match?

      -- if there is an error case, we don't do de-wild it. This is because
      -- the case might actually be impossible. If we de-wild, then GHC
      -- complains about inaccessible code.
    mk_wild_matches _ rhs
      | (DVarE err) `DAppE` _ <- rhs
      , err == errorName
      = return [DMatch DWildPa rhs]

    mk_wild_matches used@(c1:_) rhs = do
      ty_name <- dataConNameToDataName c1
      (_, cons) <- getDataD "Constructor of unknown datatype" ty_name
      dcons <- mapM dsCon cons
      let used_set    = Set.fromList used
          unused_cons = filter (`name_not_in` used_set) dcons
          new_matches = map (con_to_match rhs) unused_cons
      return new_matches

    name_not_in (DCon _ _ con_name _) name_set = not (con_name `Set.member` name_set)

    con_to_match rhs con =
      let (con_name, num_fields) = extractNameArgs con in
      DMatch (DConPa con_name (replicate num_fields DWildPa)) rhs

freshenNames :: Quasi q => [DLetDec] -> q [DLetDec]
freshenNames decs = snd `liftM` bind_let_decs mempty decs
  where
    go env (DVarE n) = return $ DVarE $ use_name env n
    go env (DAppE e1 e2) = DAppE <$> go env e1 <*> (go env e2)
    go env (DLamE ns e) = do (env', ns') <- mapAccumLM bind env ns
                             DLamE ns' <$> go env' e
    go env (DCaseE e matches) =
      DCaseE <$> go env e <*> mapM (go_match env) matches
    go env (DLetE let_decs e) = do
      (env', let_decs') <- bind_let_decs env let_decs
      DLetE let_decs' <$> go env' e
    go env (DSigE e t) = DSigE <$> go env e <*> pure t
    go _ e = return e

    go_match env (DMatch pat e) = do
      (env', pat') <- bind_pat env pat
      DMatch pat' <$> go env' e

    bind (in_scope, mapping) n
      | Set.member (nameBase n) in_scope
      = do n' <- newUniqueName (nameBase n)
           return ((in_scope, Map.insert n n' mapping), n')
      | otherwise
      = return ((Set.insert (nameBase n) in_scope, mapping), n)

    bind_pat env (DVarPa n) = second DVarPa `liftM` bind env n
    bind_pat env (DConPa con_name pats) = do
      (env', pats') <- mapAccumLM bind_pat env pats
      return (env', DConPa con_name pats')
    bind_pat env (DTildePa pat) = second DTildePa `liftM` bind_pat env pat
    bind_pat env (DBangPa pat) = second DBangPa `liftM` bind_pat env pat
    bind_pat env pat = return (env, pat)

    bind_let_decs env let_decs = do
      (env', let_decs1) <- mapAccumLM bind_let_dec_lhs env let_decs
      let_decs2 <- mapM (go_let_dec_rhs env') let_decs1
      return (env', let_decs2)

    bind_let_dec_lhs env (DFunD n clauses) = do
      (env', n') <- bind env n
      return (env', DFunD n' clauses)
    bind_let_dec_lhs env (DValD pat exp) = do
      (env', pat') <- bind_pat env pat
      return (env', DValD pat' exp)
    bind_let_dec_lhs env dec = return (env, dec)
      -- don't look at infix or sigs yet!

    go_let_dec_rhs env (DFunD n clauses) =
      DFunD n <$> mapM (go_clause env) clauses
    go_let_dec_rhs env (DValD pat exp) =
      DValD pat <$> go env exp
    go_let_dec_rhs env (DSigD n ty) = return $ DSigD (use_name env n) ty
    go_let_dec_rhs env (DInfixD fix n) = return $ DInfixD fix (use_name env n)

    go_clause env (DClause pats exp) = do
      (env', pats') <- mapAccumLM bind_pat env pats
      DClause pats' <$> go env' exp

    use_name (_, mapping) n = case Map.lookup n mapping of
      Just n' -> n'
      Nothing -> n

mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining funcction
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')
