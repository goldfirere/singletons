{- Data/Singletons/Promote/Monad.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

This file defines the PrM monad and its operations, for use during promotion.

The PrM monad allows reading from a PrEnv environment and writing to a list
of DDec, and is wrapped around a Q.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, CPP,
             FlexibleContexts #-}

module Data.Singletons.Promote.Monad (
  PrM, promoteM, promoteMDecs, 
  allLocals, emitDecs, emitDecsM,
  lambdaBind, LetBind, letBind, lookupVarE,
  LetDecEnv, LetDecRHS(..), valueBinding, typeBinding, emptyLetDecEnv
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.Set as Set
import Data.Set ( Set )
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Data.Singletons.Util
import Control.Applicative
import Data.Singletons.Names

-- environment during promotion
data PrEnv =
  PrEnv { pr_lambda_bound :: Set Name       -- these type variables are in scope
        , pr_let_bound    :: Map Name DType -- these variables must be expanded
        }

emptyPrEnv :: PrEnv
emptyPrEnv = PrEnv { pr_lambda_bound = Set.empty
                   , pr_let_bound    = Map.empty }

-- the promotion monad
newtype PrM a = PrM (ReaderT PrEnv (WriterT [DDec] Q) a)
  deriving ( Functor, Applicative, Monad, Quasi
           , MonadReader PrEnv, MonadWriter [DDec] )

-- we need Quasi instances for ReaderT and WriterT for the above to work.

instance (Quasi q, Monoid m) => Quasi (WriterT m q) where
  qNewName          = lift `comp1` qNewName
  qReport           = lift `comp2` qReport
  qLookupName       = lift `comp2` qLookupName
  qReify            = lift `comp1` qReify
  qReifyInstances   = lift `comp2` qReifyInstances
  qLocation         = lift qLocation
  qRunIO            = lift `comp1` qRunIO
  qAddDependentFile = lift `comp1` qAddDependentFile
#if __GLASGOW_HASKELL__ >= 707
  qReifyRoles       = lift `comp1` qReifyRoles
  qReifyAnnotations = lift `comp1` qReifyAnnotations
  qReifyModule      = lift `comp1` qReifyModule
  qAddTopDecls      = lift `comp1` qAddTopDecls
  qAddModFinalizer  = lift `comp1` qAddModFinalizer
  qGetQ             = lift qGetQ
  qPutQ             = lift `comp1` qPutQ
#endif

  qRecover exp handler = do
    (result, aux) <- lift $ qRecover (runWriterT exp) (runWriterT handler)
    tell aux
    return result

instance Quasi q => Quasi (ReaderT r q) where
  qNewName          = lift `comp1` qNewName
  qReport           = lift `comp2` qReport
  qLookupName       = lift `comp2` qLookupName
  qReify            = lift `comp1` qReify
  qReifyInstances   = lift `comp2` qReifyInstances
  qLocation         = lift qLocation
  qRunIO            = lift `comp1` qRunIO
  qAddDependentFile = lift `comp1` qAddDependentFile
#if __GLASGOW_HASKELL__ >= 707
  qReifyRoles       = lift `comp1` qReifyRoles
  qReifyAnnotations = lift `comp1` qReifyAnnotations
  qReifyModule      = lift `comp1` qReifyModule
  qAddTopDecls      = lift `comp1` qAddTopDecls
  qAddModFinalizer  = lift `comp1` qAddModFinalizer
  qGetQ             = lift qGetQ
  qPutQ             = lift `comp1` qPutQ
#endif

  qRecover exp handler = do
    env <- ask
    lift $ qRecover (runReaderT exp env) (runReaderT handler env)

allLocals :: MonadReader PrEnv m => m [Name]
allLocals = do
  lambdas <- asks (Set.elems . pr_lambda_bound)
  lets    <- asks pr_let_bound
    -- filter out shadowed variables!
  return [ name
         | name <- lambdas
         , case Map.lookup name lets of
             Just (DVarT name') | name' == name -> True
             _                                  -> False ]

emitDecs :: MonadWriter [DDec] m => [DDec] -> m ()
emitDecs = tell

emitDecsM :: MonadWriter [DDec] m => m [DDec] -> m ()
emitDecsM action = do
  decs <- action
  emitDecs decs

-- when lambda-binding variables, we still need to add the variables
-- to the let-expansion, because of shadowing. ugh.
lambdaBind :: [Name] -> PrM a -> PrM a
lambdaBind binds = local add_binds
  where add_binds env@(PrEnv { pr_lambda_bound = lambdas
                             , pr_let_bound    = lets }) =
          let new_lets = Map.fromList [ (n, DVarT n) | n <- binds ] in
          env { pr_lambda_bound = Set.union (Set.fromList binds) lambdas
              , pr_let_bound    = Map.union new_lets lets }

type LetBind = (Name, DType)
letBind :: [LetBind] -> PrM a -> PrM a
letBind binds = local add_binds
  where add_binds env@(PrEnv { pr_let_bound = lets }) =
          env { pr_let_bound = Map.union (Map.fromList binds) lets }

lookupVarE :: Name -> PrM DType
lookupVarE n = do
  lets <- asks pr_let_bound
  case Map.lookup n lets of
    Just ty -> return ty
    Nothing -> return $ promoteValRhs n

promoteM :: Quasi q => PrM a -> q (a, [DDec])
promoteM (PrM reader) =
  let writer = runReaderT reader emptyPrEnv
      q      = runWriterT writer
  in
  runQ q

-- promoteM specialized to [DDec]
promoteMDecs :: Quasi q => PrM [DDec] -> q [DDec]
promoteMDecs thing = do
  (decs1, decs2) <- promoteM thing
  return $ decs1 ++ decs2

--------------------------------------------------------------------
-- PrDecM
-- A specialized monad for promoted let declarations
--------------------------------------------------------------------

data LetDecRHS = Function [DClause]
               | Value    DExp
type LetDecEnv = ( Map Name LetDecRHS
                 , Map Name DType )  -- type signatures

valueBinding :: Name -> LetDecRHS -> PrM LetDecEnv
valueBinding n v = return (Map.singleton n v, mempty)

typeBinding :: Name -> DType -> PrM LetDecEnv
typeBinding n t = return (mempty, Map.singleton n t)

emptyLetDecEnv :: LetDecEnv
emptyLetDecEnv = mempty
