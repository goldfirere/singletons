{- Data/Singletons/Single/Type.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

Singletonizes types.
-}

module Data.Singletons.Single.Type where

import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Language.Haskell.TH.Desugar.OSet (OSet)
import Language.Haskell.TH.Syntax
import Data.Singletons.Names
import Data.Singletons.Single.Monad
import Data.Singletons.Promote.Type
import Data.Singletons.Util
import Control.Monad
import Data.Foldable

singType :: OSet Name      -- the set of bound kind variables in this scope
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
  checkVanillaDType ty
  let (_, cxt, args, res) = unravelVanillaDType ty
      num_args            = length args
  cxt' <- mapM singPred_NC cxt
  arg_names <- replicateM num_args (qNewName "t")
  prom_args <- mapM promoteType_NC args
  prom_res  <- promoteType_NC res
  let args' = map (\n -> singFamily `DAppT` (DVarT n)) arg_names
      res'  = singFamily `DAppT` (foldl apply prom (map DVarT arg_names) `DSigT` prom_res)
      tau   = ravel args' res'
      -- Make sure to subtract out the bound variables currently in scope, lest we
      -- accidentally shadow them in this type signature.
      kv_names_to_bind = foldMap fvDType (prom_args ++ cxt' ++ [prom_res])
                            OSet.\\ bound_kvs
      kvs_to_bind      = toList kv_names_to_bind
  let ty' = DForallT ForallInvis
                     (map DPlainTV kvs_to_bind ++ zipWith DKindedTV arg_names prom_args) $
            DConstrainedT cxt' tau
  return (ty', num_args, arg_names, cxt, prom_args, prom_res)

-- Single a DPred, checking that it is a vanilla type in the process.
-- See [Vanilla-type validity checking during promotion]
-- in Data.Singletons.Promote.Type.
singPred :: DPred -> SgM DPred
singPred p = do
  checkVanillaDType p
  singPred_NC p

-- Single a DPred. Does not check if the argument is a vanilla type.
-- See [Vanilla-type validity checking during promotion]
-- in Data.Singletons.Promote.Type.
singPred_NC :: DPred -> SgM DPred
singPred_NC = singPredRec []

-- The workhorse for singPred_NC.
singPredRec :: [DTypeArg] -> DPred -> SgM DPred
singPredRec _cxt (DForallT {}) =
  fail "Singling of quantified constraints not yet supported"
singPredRec _cxt (DConstrainedT {}) =
  fail "Singling of quantified constraints not yet supported"
singPredRec ctx (DAppT pr ty) = singPredRec (DTANormal ty : ctx) pr
singPredRec ctx (DAppKindT pr ki) = singPredRec (DTyArg ki : ctx) pr
singPredRec _ctx (DSigT _pr _ki) =
  fail "Singling of constraints with explicit kinds not yet supported"
singPredRec _ctx (DVarT _n) =
  fail "Singling of contraint variables not yet supported"
singPredRec ctx (DConT n)
  | n == equalityName
  = fail "Singling of type equality constraints not yet supported"
  | otherwise = do
    kis <- mapM promoteTypeArg_NC ctx
    let sName = singClassName n
    return $ applyDType (DConT sName) kis
singPredRec _ctx DWildCardT = return DWildCardT  -- it just might work
singPredRec _ctx DArrowT =
  fail "(->) spotted at head of a constraint"
singPredRec _ctx (DLitT {}) =
  fail "Type-level literal spotted at head of a constraint"
