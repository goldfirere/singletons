{- Data/Singletons/Single/Squash.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

Squash top-level wildcard patterns.
-}

module Data.Singletons.Single.Squash where

import Prelude hiding ( exp )

import Language.Haskell.TH.Desugar
import Control.Applicative
import Control.Monad
import Language.Haskell.TH.Desugar.Match
import Data.Singletons.Util
import qualified Data.Set as S
import Language.Haskell.TH.Syntax

squashWildcards :: DsMonad q => DLetDec -> q DLetDec
squashWildcards = go_let_dec <=< scLetDec
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
      
    mk_wild_matches used@(c1:_) rhs = do
      ty_name <- dataConNameToDataName c1
      (_, cons) <- getDataD "Constructor of unknown datatype" ty_name
      dcons <- mapM dsCon cons
      let used_set    = S.fromList used
          unused_cons = filter (`name_not_in` used_set) dcons
          new_matches = map (con_to_match rhs) unused_cons
      return new_matches

    name_not_in (DCon _ _ con_name _) name_set = not (con_name `S.member` name_set)

    con_to_match rhs con =
      let (con_name, num_fields) = extractNameArgs con in
      DMatch (DConPa con_name (replicate num_fields DWildPa)) rhs

