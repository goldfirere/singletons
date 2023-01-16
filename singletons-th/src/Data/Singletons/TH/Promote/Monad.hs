{- Data/Singletons/TH/Promote/Monad.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

This file defines the PrM monad and its operations, for use during promotion.

The PrM monad allows reading from a PrEnv environment and writing to a list
of DDec, and is wrapped around a Q.
-}

module Data.Singletons.TH.Promote.Monad (
  PrM, promoteM, promoteM_, promoteMDecs, VarPromotions,
  allLocals, emitDecs, emitDecsM,
  lambdaBind, LetBind, letBind, lookupVarE
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
import Data.Singletons.TH.Options
import Data.Singletons.TH.Syntax

type LetExpansions = OMap Name DType  -- from **term-level** name

-- environment during promotion
data PrEnv =
  PrEnv { pr_options      :: Options
        , pr_lambda_bound :: OMap Name Name
        , pr_let_bound    :: LetExpansions
        , pr_local_decls  :: [Dec]
        }

emptyPrEnv :: PrEnv
emptyPrEnv = PrEnv { pr_options      = defaultOptions
                   , pr_lambda_bound = OMap.empty
                   , pr_let_bound    = OMap.empty
                   , pr_local_decls  = [] }

-- the promotion monad
newtype PrM a = PrM (ReaderT PrEnv (WriterT [DDec] Q) a)
  deriving ( Functor, Applicative, Monad, Quasi
           , MonadReader PrEnv, MonadWriter [DDec]
           , MonadFail, MonadIO )

instance DsMonad PrM where
  localDeclarations = asks pr_local_decls

instance OptionsMonad PrM where
  getOptions = asks pr_options

-- return *type-level* names
allLocals :: MonadReader PrEnv m => m [Name]
allLocals = do
  lambdas <- asks (OMap.assocs . pr_lambda_bound)
  lets    <- asks pr_let_bound
    -- filter out shadowed variables!
  return [ typeName
         | (termName, typeName) <- lambdas
         , case OMap.lookup termName lets of
             Just (DVarT typeName') | typeName' == typeName -> True
             _                                              -> False ]

emitDecs :: MonadWriter [DDec] m => [DDec] -> m ()
emitDecs = tell

emitDecsM :: MonadWriter [DDec] m => m [DDec] -> m ()
emitDecsM action = do
  decs <- action
  emitDecs decs

-- when lambda-binding variables, we still need to add the variables
-- to the let-expansion, because of shadowing. ugh.
lambdaBind :: VarPromotions -> PrM a -> PrM a
lambdaBind binds = local add_binds
  where add_binds env@(PrEnv { pr_lambda_bound = lambdas
                             , pr_let_bound    = lets }) =
          let new_lets = OMap.fromList [ (tmN, DVarT tyN) | (tmN, tyN) <- binds ] in
          env { pr_lambda_bound = OMap.fromList binds `OMap.union` lambdas
              , pr_let_bound    = new_lets            `OMap.union` lets }

type LetBind = (Name, DType)
letBind :: [LetBind] -> PrM a -> PrM a
letBind binds = local add_binds
  where add_binds env@(PrEnv { pr_let_bound = lets }) =
          env { pr_let_bound = OMap.fromList binds `OMap.union` lets }

lookupVarE :: Name -> PrM DType
lookupVarE n = do
  opts <- getOptions
  lets <- asks pr_let_bound
  case OMap.lookup n lets of
    Just ty -> return ty
    Nothing -> return $ DConT $ defunctionalizedName0 opts n

promoteM :: OptionsMonad q => [Dec] -> PrM a -> q (a, [DDec])
promoteM locals (PrM rdr) = do
  opts         <- getOptions
  other_locals <- localDeclarations
  let wr = runReaderT rdr (emptyPrEnv { pr_options     = opts
                                      , pr_local_decls = other_locals ++ locals })
      q  = runWriterT wr
  runQ q

promoteM_ :: OptionsMonad q => [Dec] -> PrM () -> q [DDec]
promoteM_ locals thing = do
  ((), decs) <- promoteM locals thing
  return decs

-- promoteM specialized to [DDec]
promoteMDecs :: OptionsMonad q => [Dec] -> PrM [DDec] -> q [DDec]
promoteMDecs locals thing = do
  (decs1, decs2) <- promoteM locals thing
  return $ decs1 ++ decs2
