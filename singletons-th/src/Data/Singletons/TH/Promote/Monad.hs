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

-- environment during promotion
data PrEnv =
  PrEnv { pr_options     :: Options
        , pr_lambda_vars :: OMap Name Name
          -- ^ Map from term-level 'Name's of variables bound in lambdas and
          -- function clauses to their type-level counterparts.
          -- See @Note [Tracking local variables]@.
        , pr_local_vars  :: OMap Name DType
          -- ^ Map from term-level 'Name's of local variables to their
          -- type-level counterparts. See @Note [Tracking local variables]@.
        , pr_local_decls :: [Dec]
        }

emptyPrEnv :: PrEnv
emptyPrEnv = PrEnv { pr_options     = defaultOptions
                   , pr_lambda_vars = OMap.empty
                   , pr_local_vars  = OMap.empty
                   , pr_local_decls = [] }

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
  lambdas <- asks (OMap.assocs . pr_lambda_vars)
  return $ map snd lambdas

emitDecs :: MonadWriter [DDec] m => [DDec] -> m ()
emitDecs = tell

emitDecsM :: MonadWriter [DDec] m => m [DDec] -> m ()
emitDecsM action = do
  decs <- action
  emitDecs decs

-- ^ Bring a list of 'VarPromotions' into scope for the duration the supplied
-- computation. See @Note [Tracking local variables]@.
lambdaBind :: VarPromotions -> PrM a -> PrM a
lambdaBind binds = local add_binds
  where add_binds env@(PrEnv { pr_lambda_vars = lambdas
                             , pr_local_vars  = locals }) =
          -- Per Note [Tracking local variables], these will be added to both
          -- `pr_lambda_vars` and `pr_local_vars`.
          let new_locals = OMap.fromList [ (tmN, DVarT tyN) | (tmN, tyN) <- binds ] in
          env { pr_lambda_vars = OMap.fromList binds `OMap.union` lambdas
              , pr_local_vars  = new_locals          `OMap.union` locals }

-- ^ A pair consisting of a term-level 'Name' of a variable, bound in a @let@
-- binding or @where@ clause, and its type-level counterpart.
-- See @Note [Tracking local variables]@.
type LetBind = (Name, DType)

-- ^ Bring a list of 'LetBind's into scope for the duration the supplied
-- computation. See @Note [Tracking local variables]@.
letBind :: [LetBind] -> PrM a -> PrM a
letBind binds = local add_binds
  where add_binds env@(PrEnv { pr_local_vars = locals }) =
          env { pr_local_vars = OMap.fromList binds `OMap.union` locals }

-- | Map a term-level 'Name' to its type-level counterpart. This function is
-- aware of any local variables that are currently in scope.
-- See @Note [Tracking local variables]@.
lookupVarE :: Name -> PrM DType
lookupVarE n = do
  opts <- getOptions
  locals <- asks pr_local_vars
  case OMap.lookup n locals of
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

{-
Note [Tracking local variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Handling local variables in singletons-th requires some care. There are two
sorts of local variables that singletons-th tracks:

1. Lambda-bound variables, e.g.,

     f = \x -> x
     g x = x

   In both `f` and `g`, the variable `x` is considered lambda-bound.

2. Let-bound variables, e.g.,

     h =
       let x = 42 in
       x + x

     i = x + x
       where
         x = 42

   In both `h` and `i`, the variable `x` is considered let-bound.

Why does singletons-th need to track local variables? It's because they must
be promoted differently depending on whether they are local or not. Consider:

  j = ... x ...

When promoting the `j` function to a type family `J`, there are three possible
ways of promoting `x`:

* If `x` is a lambda-bound variable, then `x` must be promoted to a type
  variable. In general, we cannot promote `x` to the same name. Consider this
  example:

    j (%%) x y = x %% y

  Here, `(%%)`, `x`, and `y` are lambda-bound variables. But we cannot promote
  `j` to this type family:

    type family J (%%) x y where
      J (%%) x y = x %% y

  This is because type variable names cannot be symbolic like `(%%)` is. As a
  result, we create a fresh name `ty` and promote each occurrence of `(%%)` to
  `ty`:

    type family J ty x y where
      J ty x y = x `ty` y

  See `mkTyName` in Data.Singletons.TH.Names. In fact, `mkTyName` will also
  freshen alphanumeric names, so it would be more accurate to say that `j` will
  be promoted to this:

    type family J ty x_123 y_456 where
      J ty x_123 y_456 = x_123 `ty` y_456

  Where `x_123` and `y_456` are fresh names that are distinct from `x` and `y`.
  Freshening alphanumeric names like `x` and `y` is probably not strictly
  necessary, but `mkTyName` does it anyway (1) for consistency with symbolic
  names and (2) to make the type-level names easier to tell apart from the
  original term-level names.

* If `x` is a let-bound variable, then `x` must be promoted to something like
  `LetX`, where `LetX` is the lambda-lifted version of `x`. For instance, we
  would promote this:

    j = x
      where
        x = True

  To this:

    type family J where
      J = LetX
    type family LetX where
      LetX = True

* If `x` is not a local variable at all, then `x` must be promoted to something
  like `X`, which is assumed to be a top-level function. For instance, we would
  promote this:

    x = 42
    j = x

  To this:

    type family X where
      X = 42
    type family J where
      J = X

Being able to distinguish between all these sorts of variables requires
recording whether they are lambda-/let-bound at their binding sites during
promotion and singling. During promotion, the `pr_local_vars` field of `PrEnv`
is responsible for this. In singling, the `sg_local_vars` field of `SgEnv` is
responsible for this. Each of these fields are Maps from the original,
term-level Names to the promoted or singled versions of the Names. The
`lookupVarE` functions (which can be found in both
Data.Singletons.TH.Promote.Monad and Data.Singletons.TH.Single.Monad) are
responsible for determining what a term-level Name should be mapped to.

In addition to `pr_local_vars` and `sg_local_vars`, which include both lambda-
and let-bound variables, `PrEnv` also includes a separate `pr_lambda_vars`
field, which only tracks lambda-bound variables. We must do this because
lambda-bound variables are treated differently during lambda lifting.
Lambda-lifted functions must close over any lambda-bound variables in scope,
but /not/ any let-bound variables in scope, since the latter are lambda-lifted
separately. (Because singling does not do anything akin to lambda lifting,
`SgEnv` does not have anything like `sg_lambda_vars`.)

A consequence of this is that when we lambda-bind a variable during promotion
(see `lambdaBind`), we add the variable to both `pr_lambda_vars` and
`pr_local_vars`.  When we let-bind a variable during promotion (see `letBind`),
we only add the variable to `pr_local_vars`. This means that `pr_lambda_vars`
will always be a subset of `pr_local_vars`.
-}
