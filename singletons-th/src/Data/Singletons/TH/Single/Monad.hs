{-# LANGUAGE TemplateHaskellQuotes #-}

{- Data/Singletons/TH/Single/Monad.hs

(c) Richard Eisenberg 2014
rae@cs.brynmawr.edu

This file defines the SgM monad and its operations, for use during singling.

The SgM monad allows reading from a SgEnv environment and is wrapped around a Q.
-}

module Data.Singletons.TH.Single.Monad (
  SgM, bindLambdas, bindLets, bindContext,
  askContext, lookupVarE, lookupConE,
  wrapSingFun,
  singM, singDecsM,
  emitDecs, emitDecsM
  ) where

import Prelude hiding ( exp )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Singletons
import Data.Singletons.TH.Options
import Data.Singletons.TH.Promote.Monad ( emitDecs, emitDecsM )
import Data.Singletons.TH.Util
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Control.Monad ( liftM2 )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( MonadReader(..), ReaderT(..), asks )
import Control.Monad.Writer ( MonadWriter, WriterT(..) )
import Control.Applicative

-- environment during singling
data SgEnv =
  SgEnv { sg_options     :: Options
        , sg_local_vars  :: Map Name DExp
          -- ^ Map from term-level 'Name's of local variables to their
          -- singled counterparts. See @Note [Tracking local variables]@ in
          -- "Data.Singletons.TH.Promote.Monad".
        , sg_context     :: DCxt -- See Note [Tracking the current type signature context]
        , sg_local_decls :: [Dec]
        }

emptySgEnv :: SgEnv
emptySgEnv = SgEnv { sg_options     = defaultOptions
                   , sg_local_vars  = Map.empty
                   , sg_context     = []
                   , sg_local_decls = []
                   }

-- the singling monad
newtype SgM a = SgM (ReaderT SgEnv (WriterT [DDec] Q) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader SgEnv, MonadWriter [DDec]
           , MonadFail, MonadIO, Quasi )

instance DsMonad SgM where
  localDeclarations = asks sg_local_decls

instance OptionsMonad SgM where
  getOptions = asks sg_options

-- ^ Bring a list of lambda-bound names into scope for the duration the supplied
-- computation, where the first element of each pair is the original, term-level
-- name, and the second element of each pair is the singled counterpart.
-- See @Note [Tracking local variables]@ in "Data.Singletons.TH.Promote.Monad".
bindLambdas :: [(Name, Name)] -> SgM a -> SgM a
bindLambdas lambdas = local add_binds
  where add_binds env@(SgEnv { sg_local_vars = locals }) =
          let new_locals = Map.fromList [ (tmN, DVarE tyN) | (tmN, tyN) <- lambdas ] in
          env { sg_local_vars = new_locals `Map.union` locals }

-- ^ Bring a list of let-bound names into scope for the duration the supplied
-- computation, where the first element of each pair is the original, term-level
-- name, and the second element of each pair is the singled counterpart.
-- See @Note [Tracking local variables]@ in "Data.Singletons.TH.Promote.Monad".
bindLets :: [(Name, DExp)] -> SgM a -> SgM a
bindLets lets =
  local (\env@(SgEnv { sg_local_vars = locals }) ->
               env { sg_local_vars = Map.fromList lets `Map.union` locals })

-- Add some constraints to the current type signature context.
-- See Note [Tracking the current type signature context]
bindContext :: DCxt -> SgM a -> SgM a
bindContext ctxt1
  = local (\env@(SgEnv { sg_context = ctxt2 }) ->
                 env { sg_context = ctxt1 ++ ctxt2 })

-- Retrieve the current type signature context.
-- See Note [Tracking the current type signature context]
askContext :: SgM DCxt
askContext = asks sg_context

-- | Map a term-level 'Name' to its singled counterpart. This function is aware
-- of any local variables that are currently in scope.
-- See @Note [Tracking local variables]@ in "Data.Singletons.TH.Promote.Monad".
lookupVarE :: Name -> SgM DExp
lookupVarE name = do
  opts <- getOptions
  lookup_var_con (singledValueName opts)
                 (DVarE . singledValueName opts) name

-- | Map a data constructor name to its singled counterpart.
lookupConE :: Name -> SgM DExp
lookupConE name = do
  opts <- getOptions
  lookup_var_con (singledDataConName opts)
                 (DConE . singledDataConName opts) name

lookup_var_con :: (Name -> Name) -> (Name -> DExp) -> Name -> SgM DExp
lookup_var_con mk_sing_name mk_exp name = do
  opts <- getOptions
  localExpansions <- asks sg_local_vars
  sName <- mkDataName (nameBase (mk_sing_name name)) -- we want *term* names!
  case Map.lookup name localExpansions of
    Nothing -> do
      -- try to get it from the global context
      m_dinfo <- liftM2 (<|>) (dsReify sName) (dsReify name)
        -- try the unrefined name too -- it's needed to bootstrap Enum
      case m_dinfo of
        Just (DVarI _ ty _) ->
          let num_args = countArgs ty in
          return $ wrapSingFun num_args (DConT $ defunctionalizedName0 opts name)
                               (mk_exp name)
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

singM :: OptionsMonad q => [Dec] -> SgM a -> q (a, [DDec])
singM locals (SgM rdr) = do
  opts         <- getOptions
  other_locals <- localDeclarations
  let wr = runReaderT rdr (emptySgEnv { sg_options     = opts
                                      , sg_local_decls = other_locals ++ locals })
      q  = runWriterT wr
  runQ q

singDecsM :: OptionsMonad q => [Dec] -> SgM [DDec] -> q [DDec]
singDecsM locals thing = do
  (decs1, decs2) <- singM locals thing
  return $ decs1 ++ decs2

{-
Note [Tracking the current type signature context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Much like we track the locally-bound names in scope (see Note [Tracking local
variables] in Data.Singletons.TH.Promote.Monad), we also track the current
context. For instance, in the following program:

  -- (1)
  f :: forall a. Show a => a -> String -> Bool
  f x y = g (show x) y
    where
      -- (2)
      g :: forall b. Eq b => b -> b -> Bool
      g = h
        where
          -- (3)
          h :: b -> b -> Bool
          h = (==)

Here is the context at various points:

(1) ()
(2) (Show a)
(3) (Show a, Eq b)

We track this informating during singling instead of during promotion, as the
promoted versions of things are often type families, which do not have
contexts.

Why do we bother tracking this at all? Ultimately, because singDefuns (from
Data.Singletons.TH.Single.Defun) needs to know the current context in order to
generate a correctly typed SingI instance. For instance, if you called
singDefuns on the class method bar:

  class Foo a where
    bar :: Eq a => a -> Bool

Then if you only grabbed the context of `bar` itself, then you'd end up
generating the following SingI instance for BarSym0:

  instance SEq a => SingI (FooSym0 :: a ~> Bool) where ...

Which is incorrect—there needs to be an (SFoo a) constraint as well! If we
track the current context when singling Foo, then we will correctly propagate
this information to singDefuns.
-}
