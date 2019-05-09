{-# LANGUAGE ScopedTypeVariables #-}

{- Data/Singletons/Type.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file implements promotion of types into kinds.
-}

module Data.Singletons.Promote.Type
  ( promoteType, promotePred, promoteTypeArg, promoteUnraveled
  ) where

import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Language.Haskell.TH

-- the only monadic thing we do here is fail. This allows the function
-- to be used from the Singletons module
promoteType :: forall m. MonadFail m => DType -> m DKind
promoteType = go []
  where
    go :: [DTypeArg] -> DType -> m DKind
    -- We don't need to worry about constraints: they are used to express
    -- static guarantees at runtime. But, because we don't need to do
    -- anything special to keep static guarantees at compile time, we don't
    -- need to promote them.
    go []       (DForallT _tvbs _cxt ty) = go [] ty
    go []       (DAppT (DAppT DArrowT (DForallT (_:_) _ _)) _) =
      fail "Cannot promote types of rank above 1."
    go args     (DAppT t1 t2) = do
      k2 <- go [] t2
      go (DTANormal k2 : args) t1
       -- NB: This next case means that promoting something like
       --   (((->) a) :: Type -> Type) b
       -- will fail because the pattern below won't recognize the
       -- arrow to turn it into a TyFun. But I'm not terribly
       -- bothered by this, and it would be annoying to fix. Wait
       -- for someone to report.
    go args     (DAppKindT ty ki) = do
      ki' <- go [] ki
      go (DTyArg ki' : args) ty
    go args     (DSigT ty ki) = do
      ty' <- go [] ty
      -- No need to promote 'ki' - it is already a kind.
      return $ applyDType (DSigT ty' ki) args
    go args     (DVarT name) = return $ applyDType (DVarT name) args
    go []       (DConT name)
      | name == typeRepName               = return $ DConT typeKindName
      | nameBase name == nameBase repName = return $ DConT typeKindName
    go args     (DConT name)
      | Just n <- unboxedTupleNameDegree_maybe name
      = return $ applyDType (DConT (tupleTypeName n)) args
      | otherwise
      = return $ applyDType (DConT name) args
    go [DTANormal k1, DTANormal k2] DArrowT
      = return $ DConT tyFunArrowName `DAppT` k1 `DAppT` k2
    go _        ty@DLitT{} = pure ty

    go args     hd = fail $ "Illegal Haskell construct encountered:\n" ++
                            "headed by: " ++ show hd ++ "\n" ++
                            "applied to: " ++ show args

-- Make sure to keep this in sync with Data.Singletons.Single.Type.singPred
promotePred :: forall m. MonadFail m => DPred -> m DPred
promotePred = go []
  where
  go :: [DTypeArg] -> DPred -> m DPred
  go _args (DForallT {})    = fail "Cannot promote quantified constraints"
  go args (DAppT pr ty)     = go (DTANormal ty : args) pr
  go args (DAppKindT pr ki) = go (DTyArg ki : args) pr
  go args (DSigT pr _k)     = go args pr -- just ignore the kind; it can't matter
  go _args (DVarT name)     = fail $ "Cannot promote ConstraintKinds variables like "
                                  ++ show name
  go args (DConT name)      | name == equalityName
                            = fail "Cannot promote equality constraints"
                            | otherwise
                            = do args' <- traverse promoteTypeArg args
                                 let pName = promoteClassName name
                                 pure $ applyDType (DConT pName) args'
  go _args DWildCardT       = pure DWildCardT
  go _args (DLitT {})       = fail "Type-level literal spotted at head of a constraint"
  go _args DArrowT          = fail "(->) spotted at head of a constraint"

promoteTypeArg :: MonadFail m => DTypeArg -> m DTypeArg
promoteTypeArg (DTANormal t) = DTANormal <$> promoteType t
promoteTypeArg ta@(DTyArg _) = pure ta -- Kinds are already promoted

promoteUnraveled :: MonadFail m => DType -> m ([DKind], DKind)
promoteUnraveled ty = do
  arg_kis <- mapM promoteType arg_tys
  res_ki  <- promoteType res_ty
  return (arg_kis, res_ki)
  where
    (_, _, arg_tys, res_ty) = unravel ty
