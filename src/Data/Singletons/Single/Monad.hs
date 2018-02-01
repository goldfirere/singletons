{- Data/Singletons/Single/Monad.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

This file defines the SgM monad and its operations, for use during singling.

The SgM monad allows reading from a SgEnv environment and is wrapped around a Q.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, ParallelListComp, TemplateHaskell #-}

module Data.Singletons.Single.Monad (
  SgM, bindLets, bindKindVars, allBoundKindVars,
  lookupVarE, lookupConE,
  wrapSingFun, wrapUnSingFun,
  singM, singDecsM,
  emitDecs, emitDecsM
  ) where

import Prelude hiding ( exp )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
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
  SgEnv { sg_let_binds       :: Map Name DExp   -- from the *original* name
        , sg_bound_kind_vars :: Set Name -- See Note [Explicitly quantifying kinds variables]
        , sg_local_decls     :: [Dec]
        }

emptySgEnv :: SgEnv
emptySgEnv = SgEnv { sg_let_binds       = Map.empty
                   , sg_bound_kind_vars = Set.empty
                   , sg_local_decls     = []
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
  qAddForeignFile     = liftSgM `comp2` qAddForeignFile
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

-- Add to the set of bound kind variables currently in scope.
-- See Note [Explicitly binding kind variables]
bindKindVars :: Set Name -> SgM a -> SgM a
bindKindVars kvs1 =
  local (\env@(SgEnv { sg_bound_kind_vars = kvs2 }) ->
               env { sg_bound_kind_vars = kvs1 `Set.union` kvs2 })

-- Look up the set of bound kind variables currently in scope.
-- See Note [Explicitly binding kind variables]
allBoundKindVars :: SgM (Set Name)
allBoundKindVars = asks sg_bound_kind_vars

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

{-
Note [Explicitly binding kind variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
singletons is careful to generate code that explicitly quantifies every kind
variable bound by a top-level forall. For example, if we were to single the
identity function:

  identity :: forall a. a -> a
  identity x = x

We would first promote the types to kinds and generate an explicit forall
mapping fresh type variables to the promoted types (step 1):

  sIdentity :: forall (x :: a). Sing x -> Sing (Identity x)
  sIdentity sX = sX

Now, because we want to explicitly bind the kind variable `a`, we traverse
the promoted types, extract the set of all kind variables, and put
them at the front of the forall, like so (step 2):

  sIdentity :: forall a (x :: a). Sing x -> Sing (Identity x)
  sIdentity sX = sX

This approach works well enough for a simple function like identity. But
consider this more complicated example:

  f :: forall a. a -> a
  f = g
    where
      g :: a -> a
      g x = x

When singling, we would eventually end up in this spot:

  sF :: forall a (x :: a). Sing a -> Sing (F a)
  sF = sG
    where
      sG :: _
      sG x = x

What should go in place of the _? After doing step 1, we would have
sG :: forall (y :: a). Sing a -> Sing (G a), so naïvely charging forward with
step 2 would extract the kind variable `a` and give:

  sF :: forall a (x :: a). Sing a -> Sing (F a)
  sF = sG
    where
      sG :: forall a (y :: a). Sing a -> Sing (G a)
      sG x = x

But this is incorrect! The `a` bound by sF /must/ be the same one used in sG,
as per the scoping of the original `f` function. So it is not enough to extract
the kind variables from the promoted types—we must extract the /free/ kind
variables from the promoted types.

That is the role that sg_bound_kind_vars in SgEnv serves. Whenever we single a
type which binds kind variables, we add them to the sg_bound_kind_vars whenever
singling code that those kind variables scope over. When we single a type
signature that uses an explicit forall, we consult sg_bound_kind_vars to
determine which kind variables have already been bound, and subtract them from
the set of kind variables under consideration to obtain the free variables.
-}
