{- Data/Singletons/Single/Type.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

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
import qualified Data.Set as Set
import Data.Set (Set)

singType :: Set Name       -- the set of bound kind variables in this scope
                           -- see Note [Explicitly binding kind variables]
                           -- in Data.Singletons.Promote.Monad
         -> DType          -- the promoted version of the thing classified by...
         -> DType          -- ... this type
         -> SgM ( DType    -- the singletonized type
                , Int      -- the number of arguments
                , [Name]   -- the names of the tyvars used in the sing'd type
                , DCxt     -- the context of the singletonized type
                , [DKind]  -- the kinds of the argument types
                , DKind )  -- the kind of the result type
singType bound_kvs prom ty = do
  let (_, cxt, args, res) = unravel ty
      num_args            = length args
  cxt' <- mapM singPred cxt
  arg_names <- replicateM num_args (qNewName "t")
  prom_args <- mapM promoteType args
  prom_res  <- promoteType res
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldl apply prom (map DVarT arg_names) `DSigT` prom_res)
      tau   = ravel args' res'
      -- Make sure to subtract out the bound variables currently in scope, lest we
      -- accidentally shadow them in this type signature.
      kv_names_to_bind = foldMap fvDType (prom_args ++ map predToType cxt' ++ [prom_res])
                             Set.\\ bound_kvs
      kvs_to_bind      = Set.toList kv_names_to_bind
  let ty' = DForallT (map DPlainTV kvs_to_bind ++ zipWith DKindedTV arg_names prom_args)
                     cxt' tau
  return (ty', num_args, arg_names, cxt, prom_args, prom_res)

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
    return $ foldPred (DConPr sName) kis
singPredRec _ctx DWildCardPr = return DWildCardPr  -- it just might work
