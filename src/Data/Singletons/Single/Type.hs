{- Data/Singletons/Single/Type.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

Singletonizes types.
-}

{-# LANGUAGE CPP #-}

module Data.Singletons.Single.Type where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Control.Monad

singType :: DType          -- the promoted version of the thing classified by...
         -> DType          -- ... this type
         -> SgM ( DType    -- the singletonized type
                , Int      -- the number of arguments
                , [Name]   -- the names of the tyvars used in the sing'd type
                , DKind )  -- the kind of the result type
singType prom ty = do
  let (_, cxt, args, res) = unravel ty
      num_args            = length args
  cxt' <- mapM singPred cxt
  arg_names <- replicateM num_args (qNewName "t")
  prom_args <- mapM promoteType args
  prom_res  <- promoteType res
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldl apply prom (map DVarT arg_names) `DSigT` prom_res)
      tau   = ravel args' res'
  let ty' = DForallT (zipWith DKindedTV arg_names prom_args)
                     cxt' tau
  return (ty', num_args, arg_names, prom_res)

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
#if MIN_VERSION_th_desugar(1,6,0)
singPredRec _ DWildCardPr = fail "Singling of type wildcard constraint not yet supported"
#endif
