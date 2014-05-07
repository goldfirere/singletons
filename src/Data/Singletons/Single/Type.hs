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

data TopLevelFlag = TopLevel | NotTopLevel

singType :: TopLevelFlag
         -> DType          -- the promoted version of the thing classified by...
         -> DType          -- ... this type
         -> SgM ( DType    -- the singletonized type
                , Int      -- the number of arguments
                , [Name] ) -- the names of the tyvars used in the sing'd type
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
