{- Data/Singletons/Single/Type.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Singletonizes types.
-}

module Data.Singletons.Single.Type where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Control.Monad

-- Note [Singletonizing type signature]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Proces of singletonizing a type signature is conducted in two steps:
--
--  1. Prepare a singletonized (but not defunctionalized) type
--     signature. The result is returned as a function that expects
--     one type parameter. That parameter is the name of a type-level
--     equivalent (ie. a type family) of a function being promoted.
--     This is done by singTypeRec. Most of the implementation is
--     straightforward. The most interesting part is the promotion of
--     arrows (ArrowT clause). When we reach an arrow we expect that
--     both its parameters are placed within the context (this is done
--     by AppT clause). We promote the type of first parameter to a
--     kind and introduce it via kind-annotated type variable in a
--     forall. At this point arguments that are functions are
--     converted to TyFun representation. This is important for
--     defunctionalization.
--
--  2. Lift out foralls: accumulate separate foralls at the beginning
--     of type signature. So this:
--
--      forall (a :: k). Proxy a -> forall (b :: [k]). Proxy b -> SList (a ': b)
--
--     becomes:
--
--      forall (a :: k) (b :: [k]). Proxy a -> Proxy b -> SList (a ': b)
--
--     This was originally a workaround for #8031 but later this was
--     used as a part of defunctionalization algorithm. Lifting
--     foralls produces new type signature and a list of type
--     variables that represent type level functions (TyFun kind).
--
--  3. Introduce Apply and Proxy. Using the list of type variables
--     that are type level functions (see step 2) we convert each
--     application of such variable into application of Apply type
--     family. Also, for each type variable that was converted to
--     Apply we introduce a Proxy parameter. For example this
--     signature:
--
--       sEither_ ::
--         forall (t1 :: TyFun k1 k3 -> *)
--                (t2 :: TyFun k2 k3 -> *)
--                (t3 :: Either k1 k2).
--                (forall (t4 :: k1). Sing t4 -> Sing (t1 t4))
--             -> (forall (t5 :: k2). Sing t5 -> Sing (t2 t5))
--             -> Sing t3 -> Sing (Either_ t1 t2 t3)
--
--     is converted to:
--
--       sEither_ ::
--         forall (t1 :: TyFun k1 k3 -> *)
--                (t2 :: TyFun k2 k3 -> *)
--                (t3 :: Either k1 k2).
--                (forall (t4 :: k1). Proxy t1 -> Sing t4 -> Sing (Apply t1 t4))
--             -> (forall (t5 :: k2). Proxy t2 -> Sing t5 -> Sing (Apply t2 t5))
--             -> Sing t3 -> Sing (Either_ t1 t2 t3)
--
--     Note that Proxy parameters were introduced only for arguments
--     that are functions. This will require us to add extra Proxy
--     arguments when calling these functions in the function body
--     (see Note [Creating singleton functions in two stages]).
--
--  4. Steps 2 and 3 are mutually recursive, ie. we introduce Apply and Proxy
--     for each parameter in the function signature we are singletonizing. Why?
--     Because a higher order function may accept parameters that are themselves
--     higher order functions:
--
--       foo :: ((a -> b) -> a -> b) -> (a -> b)  -> a -> b
--       foo f g a = f g a
--
--     Here 'foo' is a higher order function for which we must introduce Apply
--     and Proxy, but so is 'f'. Hence the mutually recursive calls between
--     introduceApplyAndProxy and introduceApplyAndProxyWorker. Singletonized
--     foo looks like this:
--
--       sFoo :: forall (k1 :: TyFun (TyFun a b -> *) (TyFun a b -> *) -> *)
--                      (k2 :: TyFun a b -> *)
--                      (k3 :: a).
--               (forall (t1 :: TyFun a b -> *).
--                       Proxy k1 ->
--                       (forall (t2 :: a). Proxy t1 -> Sing t2 -> Sing (Apply t1 t2))
--               -> forall (t3 :: a). Sing t3 -> Sing (Apply (Apply k1 t1) t3))
--               -> (forall (t4 :: a). Proxy k2 -> Sing t4 -> Sing (Apply k2 t4))
--               -> Sing k3 -> Sing (Foo k1 k2 k3)
--       sFoo f g a = (f Proxy g) a
--
--     Luckily for us the Proxies we introduce for the higher-order parameter
--     are not reflected in the body of sFoo - it is assumed that 'f' will
--     handle passing Proxy paramters to 'g' internally. This allows us to
--     discard the Proxy count returned by introduceApplyAndProxy in the body of
--     introduceApplyAndProxyWorker.

data TopLevelFlag = TopLevel | NotTopLevel

-- The return type of singType is:
--
--   Type -> (Type, [Int])
--
-- where first Type is the type that will be substituted in the
-- signature (see Note [Singletonizing type signature]). The result is
-- a tuple containing the final type signature with its proxy table.
singType :: TopLevelFlag -> DType -> DType -> SgM (DType, Int, [Name])
singType top_level prom ty = do
  let (cxt, tys) = unravel ty
      args       = init tys
      num_args   = length args
  cxt' <- mapM singPred cxt
  arg_names <- replicateM num_args (qNewName "t")
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldl apply prom (map DVarT arg_names))
      tau   = ravel (args' ++ [res'])
  ty' <- case top_level of
           TopLevel -> do
             prom_args <- mapM promoteType args
             return $ DForallT (zipWith DKindedTV arg_names prom_args)
                               cxt' tau
                -- don't annotate kinds. Why? Because the original source
                -- may have used scoped type variables, and we're just
                -- not clever enough to get the scoped kind variables right.
                -- (the business in bindTyVars gets in the way)
                -- If ScopedTypeVariables was actually sane in patterns,
                -- this restriction might be able to be lifted.
           NotTopLevel -> return $ DForallT (map DPlainTV arg_names)
                                            cxt' tau
  return (ty', num_args, arg_names)

singPred :: DPred -> SgM DPred
singPred = singPredRec []

singPredRec :: [DType] -> DPred -> SgM DPred
singPredRec ctx (DAppPr pr ty) = singPredRec (ty : ctx) pr
singPredRec _ctx (DSigPr _pr _ki) =
  fail "Singling of constraints with explicit kinds not yet supported"
singPredRec _ctx (DVarPr _n) =
  fail "Singling of contraint variables not yet supported"
singPredRec ctx (DConPr n)
  | n == equalityName
  = fail "Singling of type equality constraints not yet supported"
  | otherwise = do
    kis <- mapM promoteType ctx
    let sName = singClassName n
    return $ foldl DAppPr (DConPr sName) (map kindParam kis)
