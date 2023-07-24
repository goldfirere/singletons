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
  scopedBind, lambdaBind, LetBind, letBind, lookupVarE
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Foldable as F
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Language.Haskell.TH.Desugar.OSet (OSet)
import Data.Singletons.TH.Options
import Data.Singletons.TH.Syntax

-- environment during promotion
data PrEnv =
  PrEnv { pr_options     :: Options
        , pr_scoped_vars :: OSet Name
          -- ^ The set of scoped type variables currently in scope.
          -- See @Note [Scoped type variables]@.
        , pr_lambda_vars :: OMap Name Name
          -- ^ Map from term-level 'Name's of variables bound in lambdas and
          -- function clauses to their type-level counterparts.
          -- See @Note [Tracking local variables]@.
        , pr_local_vars  :: OMap Name DType
          -- ^ Map from term-level 'Name's of local variables to their
          -- type-level counterparts. Note that scoped type variables are stored
          -- separately in 'pr_scoped_tvs'.
          -- See @Note [Tracking local variables]@.
        , pr_local_decls :: [Dec]
        }

emptyPrEnv :: PrEnv
emptyPrEnv = PrEnv { pr_options     = defaultOptions
                   , pr_scoped_vars = OSet.empty
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
  scoped <- asks (F.toList . pr_scoped_vars)
  lambdas <- asks (OMap.assocs . pr_lambda_vars)
  return $ scoped ++ map snd lambdas

emitDecs :: MonadWriter [DDec] m => [DDec] -> m ()
emitDecs = tell

emitDecsM :: MonadWriter [DDec] m => m [DDec] -> m ()
emitDecsM action = do
  decs <- action
  emitDecs decs

-- ^ Bring a list of type variables into scope for the duration the supplied
-- computation. See @Note [Tracking local variables]@ and
-- @Note [Scoped type variables]@.
scopedBind :: OSet Name -> PrM a -> PrM a
scopedBind binds =
  local (\env@(PrEnv { pr_scoped_vars = scoped }) ->
    env { pr_scoped_vars = binds `OSet.union` scoped })

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
Handling local variables in singletons-th requires some care. There are three
sorts of local variables that singletons-th tracks:

1. Scoped type variables, e.g.,

     d :: forall a. Maybe a
     d = Nothing :: Maybe a

     e (x :: a) = Nothing :: Maybe a

   In both `d` and `e`, the variable `a` in `:: Maybe a` is scoped.

2. Lambda-bound variables, e.g.,

     f = \x -> x
     g x = x

   In both `f` and `g`, the variable `x` is considered lambda-bound.

3. Let-bound variables, e.g.,

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

When promoting the `j` function to a type family `J`, there are four possible
ways of promoting `x`:

* If `x` is a scoped type variable, then `x` must be promoted to the same
  name. This is because promoting a type variable to a kind variable is a
  no-op. For instance, we would promote this:

    j (z :: x) = (z :: x)

  To this:

    type family J arg where
      J (z :: x) = (z :: x)

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
recording whether they are scoped, lambda-bound, or let-bound at their binding
sites during promotion and singling. This is primarily done in two places:

* During promotion, the `pr_local_vars` field of `PrEnv` tracks lambda- and
  let-bound variables.

* During singling, the `sg_local_vars` field of `SgEnv` tracks lambda- and
  let-bound variables.

Each of these fields are Maps from the original, term-level Names to the
promoted or singled versions of the Names. The `lookupVarE` functions (which
can be found in both Data.Singletons.TH.Promote.Monad and
Data.Singletons.TH.Single.Monad) are responsible for determining what a
term-level Name should be mapped to.

In addition to `pr_local_vars` and `sg_local_vars`, which include both lambda-
and let-bound variables, `PrEnv` also includes two additional fields for
tracking other sorts of local variables:

* The `pr_scoped_vars` field tracks which scoped type variables are currently
  in scope. As discussed above, promoting an occurrence of a scoped type
  variable is a no-op, and as such, we never need to use `lookupVarE` to figure
  out what a scoped type variable promotes to. As such, there is no need to put
  the scoped type variables in `pr_local_vars`.

  On the other hand, we /do/ need to track the scoped type variables for
  lambda-lifting purposes (see Note [Scoped type variables]), and this is the
  only reason why we bother maintaining the `pr_scoped_vars` field in the first
  place. See the `scopedBind` function, which is responsible for adding new
  scoped type variables to `pr_scoped_vars`.

* The `pr_lambda_vars` field only tracks lambda-bound variables, unlike
  `pr_local_vars`, which also includes let-bound variables. We must do this
  because lambda-bound variables are treated differently during lambda lifting.
  Lambda-lifted functions must close over any lambda-bound variables in scope,
  but /not/ any let-bound variables in scope, since the latter are
  lambda-lifted separately.

  A consequence of this is that when we lambda-bind a variable during promotion
  (see `lambdaBind`), we add the variable to both `pr_lambda_vars` and
  `pr_local_vars`.  When we let-bind a variable during promotion (see
  `letBind`), we only add the variable to `pr_local_vars`. This means that
  `pr_lambda_vars` will always be a subset of `pr_local_vars`.

Because singling does not do anything akin to lambda lifting, `SgEnv` does not
have anything like `sg_scoped_vars` or `sg_lambda_vars`.

Note [Scoped type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scoped type variables are a particular form of local variable (see Note
[Tracking local variables]). They are arguably the trickiest form of local
variable to handle, and as noted in the singletons README, there are still some
forms of scoped type variables that singletons-th cannot handle during
promotion.

First, let's discuss how singletons-th promotes scoped type variables in
general:

* When promoting a function with a top-level type signature, we annotate each
  argument on the left-hand sides of type family equations with its kind. This
  is usually redundant, but it can sometimes be useful for bringing type
  variables into scope. For example, this:

    f :: forall a. a -> Maybe a
    f x = (Just x :: Maybe a)

  Will be promoted to something like this:

    type F :: forall a. a -> Maybe a
    type family F x where
      F (x :: a) = (Just x :: Maybe a)

  Note that we gave the `x` on the left-hand side of `F`'s equation an explicit
  `:: a` kind signature to ensure that the `a` on the right-hand side of the
  type family equation is in scope.

  The `promoteClause` function in Data.Singletons.TH.Promote is responsible for
  implementing this.

* Sometimes, there are no arguments available to bring type variables into
  scope. In these situations, we can sometimes use `@` in type family equations
  as an alternative. For example, this:

    g :: forall a. Maybe a
    g = (Nothing :: Maybe a)

  Will be promoted to this:

    type G :: forall a. Maybe a
    type family G where
      G @a = (Nothing :: Maybe a)

  Note the `@a` on `G`'s left-hand side. This relies on `G` having a standalone
  kind signature to work.

  The `promoteLetDecName` function in Data.Singletons.TH.Promote is responsible
  for implementing this.

* When lambda-lifting, singletons-th tracks the current set of scoped type
  variables and includes them as explicit arguments when promoting local
  definitions. For example, this:

    h :: forall a. a -> a
    h x = i
      where
        i = (x :: a)

  Will be promoted to this:

    type H :: forall a. a -> a
    type family H x where
      H @a (x :: a) = LetI a x

    type I a x where
      I a x = (x :: a)

  The `I` type family includes both `a` (a scoped type variable) and `x` (a
  lambda-bound variable) as explicit arguments to ensure that they are in scope
  on the right-hand side, which mentions both of them.

  singletons-th uses the `pr_scoped_vars` field of `PrM` to track scoped type
  variables. Whenever new scoped type variables are bound during promotion, the
  `scopedBind` function is used to add the variables to `pr_scoped_vars`.

These three tricks suffice to handle a substantial number of ways that scoped
type variables can be used. The approach is not perfect, however. Here are two
scenarios where singletons-th fails to promote scoped type variables:

* Funky pattern signatures like this one will not work:

    j :: forall a. a -> a
    j (x :: b) = b

  This is because singletons-th will attempt to promote `j` like so:

    type J :: forall a. a -> a
    type J x where
      J @a ((x :: b) :: a) = b

  But unlike in terms, GHC has no way to know that `a` and `b` are meant to
  refer to the same type variable. In order to make this work, we would need to
  substitute all occurrences of `a` with `b` in the type family equation (or
  vice versa), which seems challenging in the general case.

* Scoped type variables that are only mentioned in the return types of local
  definitions may not always work, such as in this example:

    k x = y
      where
        y :: forall b. Maybe b
        y = Nothing :: Maybe b

  singletons-th would promote `k` and `y` to the following type families:

    type K x where
      K x = LetY x

    type LetY x :: Maybe b where
      LetY x = Nothing :: Maybe b

  Note that because `LetY` closes over the `x` argument, it cannot easily be
  given a standalone kind signature, and this prevents us from writing
  `LetY @b x = ...`. Moreover, `LetY` does not have an argument that we can
  attach an explicit `:: b` signature to. (Attaching it to `x` would be
  incorrect, as that would give `LetY` a less general kind.)

  One possible way forward here would be to give type families the ability to
  write result signatures on their left-hand sides, similar to what GHC
  proposal #228
  (https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0228-function-result-sigs.rst)
  offers:

    type LetY x :: Maybe b where
      LetY x :: Maybe b = Nothing :: Maybe b
-}
