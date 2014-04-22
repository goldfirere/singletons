{- Data/Singletons/Single/Monad.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

This file defines the SgM monad and its operations, for use during singling.

The SgM monad allows reading from a SgEnv environment and is wrapped around a Q.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, ParallelListComp,
             TemplateHaskell #-}

module Data.Singletons.Single.Monad (
  SgM, bindLets, bindTyVars, bindTyVarsClause, lookupVarE, lookupConE,
  wrapSingFun, wrapUnSingFun,
  singM, singDecsM,
  emitDecs, emitDecsM
  ) where

import Prelude hiding ( exp )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Singletons.Promote.Monad hiding (lookupVarE)
import Data.Singletons.Names
import Data.Singletons.Util
import Data.Singletons
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Control.Applicative
import Data.Traversable ( traverse )
import Control.Monad.Reader
import Control.Monad.Writer

-- environment during singling
data SgEnv =
  SgEnv { sg_let_binds :: Map Name DExp   -- from the *original* name
        }

emptySgEnv :: SgEnv
emptySgEnv = SgEnv { sg_let_binds = Map.empty
                   }

-- the singling monad
newtype SgM a = SgM (ReaderT SgEnv PrM a)
  deriving ( Functor, Applicative, Monad, Quasi
           , MonadReader SgEnv, MonadWriter [DDec] )

bindLets :: [(Name, DExp)] -> SgM a -> SgM a
bindLets lets1 =
  local (\env@(SgEnv { sg_let_binds = lets2 }) ->
               env { sg_let_binds = (Map.fromList lets1) `Map.union` lets2 })

bindTyVarsClause :: VarPromotions   -- the bindings we wish to effect
                 -> [Name]          -- free variables in...
                 -> DType           -- ...this type of the thing_inside
                 -> [(DType, DType)]  -- and asserting these equalities
                 -> SgM DExp -> SgM DExp
bindTyVarsClause var_proms fv_names prom_fun equalities thing_inside = do
  lambda <- qNewName "lambda"
  let (term_names, tyvar_names) = unzip var_proms
      eq_ct  = [ DConPr equalityName `DAppPr` t1 `DAppPr` t2
               | (t1, t2) <- equalities ]
      ty_sig = DSigD lambda $
               DForallT (map DPlainTV tyvar_names)
                        []
                        (DForallT (map DPlainTV fv_names) eq_ct $
                                   ravel (map (\tv_name -> singFamily `DAppT` DVarT tv_name)
                                    tyvar_names
                                ++ [singFamily `DAppT` prom_fun]))
  arg_names <- mapM (qNewName . nameBase) term_names
  body <- bindLets [ (term_name, DVarE arg_name)
                   | term_name <- term_names
                   | arg_name <- arg_names ] $ thing_inside
  let fundef   = DFunD lambda [DClause (map DVarPa arg_names) body]
      let_body = foldExp (DVarE lambda) (map (DVarE . singValName) term_names)
  return $ DLetE [ty_sig, fundef] let_body

bindTyVars :: VarPromotions
           -> [Name]
           -> DType
           -> SgM DExp -> SgM DExp
bindTyVars var_proms fv_names prom_fun =
  bindTyVarsClause var_proms fv_names prom_fun []

lookupVarE :: Name -> SgM DExp
lookupVarE = lookup_var_con singValName (DVarE . singValName)

lookupConE :: Name -> SgM DExp
lookupConE = lookup_var_con singDataConName (DConE . singDataConName)

lookup_var_con :: (Name -> Name) -> (Name -> DExp) -> Name -> SgM DExp
lookup_var_con mk_sing_name mk_exp name = do
  letExpansions <- asks sg_let_binds
  sName <- mkDataName (nameBase (mk_sing_name name)) -- we want *term* names!
  case Map.lookup name letExpansions of
    Nothing -> do
      -- try to get it from the global context
      m_info <- qRecover (return Nothing) (fmap Just $ qReify sName)
      m_dinfo <- traverse dsInfo m_info
      case m_dinfo of
        Just (DVarI _ ty _ _) ->
          let num_args = countArgs ty in
          return $ wrapSingFun num_args (promoteValRhs name) (mk_exp name)
        _ -> return $ mk_exp name   -- lambda-bound
    Just exp -> return exp

wrapSingFun :: Int -> DType -> DExp -> DExp
wrapSingFun 0 _  = id
wrapSingFun n ty =
  let wrap_fun = DVarE $ case n of
                           1 -> 'singFun1
                           2 -> 'singFun2
                           3 -> 'singFun3
                           4 -> 'singFun4
                           5 -> 'singFun5
                           6 -> 'singFun6
                           7 -> 'singFun7
                           _ -> error "No support for functions of arity > 7."
  in
  (wrap_fun `DAppE` proxyFor ty `DAppE`)

wrapUnSingFun :: Int -> DType -> DExp -> DExp
wrapUnSingFun 0 _  = id
wrapUnSingFun n ty =
  let unwrap_fun = DVarE $ case n of
                             1 -> 'unSingFun1
                             2 -> 'unSingFun2
                             3 -> 'unSingFun3
                             4 -> 'unSingFun4
                             5 -> 'unSingFun5
                             6 -> 'unSingFun6
                             7 -> 'unSingFun7
                             _ -> error "No support for functions of arity > 7."
  in
  (unwrap_fun `DAppE` proxyFor ty `DAppE`)

singM :: Quasi q => SgM a -> q (a, [DDec])
singM (SgM rdr) =
  let prm = runReaderT rdr emptySgEnv in
  promoteM prm

singDecsM :: Quasi q => SgM [DDec] -> q [DDec]
singDecsM thing = do
  (decs1, decs2) <- singM thing
  return $ decs1 ++ decs2
