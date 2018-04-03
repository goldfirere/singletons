{- Data/Singletons/Single/Monad.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

This file defines the SgM monad and its operations, for use during singling.

The SgM monad allows reading from a SgEnv environment and is wrapped around a Q.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, ParallelListComp, TemplateHaskell, CPP #-}

module Data.Singletons.Single.Monad (
  SgM, bindLets, lookupVarE, lookupConE,
  wrapSingFun, wrapUnSingFun,
  singM, singDecsM,
  emitDecs, emitDecsM
  ) where

import Prelude hiding ( exp )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Singletons.Promote.Monad ( emitDecs, emitDecsM )
import Data.Singletons.Names
import Data.Singletons.Util
import Data.Singletons.Internal
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Control.Monad.Fail

-- environment during singling
data SgEnv =
  SgEnv { sg_let_binds   :: Map Name DExp   -- from the *original* name
        , sg_local_decls :: [Dec]
        }

emptySgEnv :: SgEnv
emptySgEnv = SgEnv { sg_let_binds   = Map.empty
                   , sg_local_decls = []
                   }

-- the singling monad
newtype SgM a = SgM (ReaderT SgEnv (WriterT [DDec] Q) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader SgEnv, MonadWriter [DDec]
           , MonadFail, MonadIO )

liftSgM :: Q a -> SgM a
liftSgM = SgM . lift . lift

instance Quasi SgM where
  qNewName          = liftSgM `comp1` qNewName
  qReport           = liftSgM `comp2` qReport
  qLookupName       = liftSgM `comp2` qLookupName
  qReify            = liftSgM `comp1` qReify
  qReifyInstances   = liftSgM `comp2` qReifyInstances
  qLocation         = liftSgM qLocation
  qRunIO            = liftSgM `comp1` qRunIO
  qAddDependentFile = liftSgM `comp1` qAddDependentFile
  qReifyRoles       = liftSgM `comp1` qReifyRoles
  qReifyAnnotations = liftSgM `comp1` qReifyAnnotations
  qReifyModule      = liftSgM `comp1` qReifyModule
  qAddTopDecls      = liftSgM `comp1` qAddTopDecls
  qAddModFinalizer  = liftSgM `comp1` qAddModFinalizer
  qGetQ             = liftSgM qGetQ
  qPutQ             = liftSgM `comp1` qPutQ

  qReifyFixity        = liftSgM `comp1` qReifyFixity
  qReifyConStrictness = liftSgM `comp1` qReifyConStrictness
  qIsExtEnabled       = liftSgM `comp1` qIsExtEnabled
  qExtsEnabled        = liftSgM qExtsEnabled
#if MIN_VERSION_template_haskell(2,14,0)
  qAddForeignFilePath = liftSgM `comp2` qAddForeignFilePath
  qAddTempFile        = liftSgM `comp1` qAddTempFile
#else
  qAddForeignFile     = liftSgM `comp2` qAddForeignFile
#endif
  qAddCorePlugin      = liftSgM `comp1` qAddCorePlugin

  qRecover (SgM handler) (SgM body) = do
    env <- ask
    (result, aux) <- liftSgM $
                     qRecover (runWriterT $ runReaderT handler env)
                              (runWriterT $ runReaderT body env)
    tell aux
    return result

instance DsMonad SgM where
  localDeclarations = asks sg_local_decls

bindLets :: [(Name, DExp)] -> SgM a -> SgM a
bindLets lets1 =
  local (\env@(SgEnv { sg_let_binds = lets2 }) ->
               env { sg_let_binds = (Map.fromList lets1) `Map.union` lets2 })

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
      m_dinfo <- liftM2 (<|>) (dsReify sName) (dsReify name)
        -- try the unrefined name too -- it's needed to bootstrap Enum
      case m_dinfo of
        Just (DVarI _ ty _) ->
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
  (wrap_fun `DAppTypeE` ty `DAppE`)

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
  (unwrap_fun `DAppTypeE` ty `DAppE`)

singM :: DsMonad q => [Dec] -> SgM a -> q (a, [DDec])
singM locals (SgM rdr) = do
  other_locals <- localDeclarations
  let wr = runReaderT rdr (emptySgEnv { sg_local_decls = other_locals ++ locals })
      q  = runWriterT wr
  runQ q

singDecsM :: DsMonad q => [Dec] -> SgM [DDec] -> q [DDec]
singDecsM locals thing = do
  (decs1, decs2) <- singM locals thing
  return $ decs1 ++ decs2
