{- Data/Singletons/TH/Promote/Type.hs

(c) Richard Eisenberg 2013
rae@cs.brynmawr.edu

This file implements promotion of types into kinds.
-}

module Data.Singletons.TH.Promote.Type
  ( promoteType, promoteType_NC
  , promoteTypeArg_NC, promoteUnraveled
  ) where

import Language.Haskell.TH.Desugar
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Util

-- Promote a DType to the kind level.
promoteType :: OptionsMonad m => DType -> m DKind
promoteType ty = do
  checkVanillaDType ty
  promoteType_NC ty

-- Promote a DType to the kind level. This is suffixed with "_NC" because
-- we do not invoke checkVanillaDType here.
-- See [Vanilla-type validity checking during promotion].
promoteType_NC :: OptionsMonad m => DType -> m DKind
promoteType_NC = go []
  where
    go :: OptionsMonad m => [DTypeArg] -> DType -> m DKind
    go []       (DForallT tele ty) = do
      ty' <- go [] ty
      pure $ DForallT tele ty'
    -- We don't need to worry about constraints: they are used to express
    -- static guarantees at runtime. But, because we don't need to do
    -- anything special to keep static guarantees at compile time, we don't
    -- need to promote them.
    go []       (DConstrainedT _cxt ty) = go [] ty
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
    go args     (DConT name) = do
      opts <- getOptions
      return $ applyDType (DConT (promotedDataTypeOrConName opts name)) args
    go [DTANormal k1, DTANormal k2] DArrowT
      = return $ DConT tyFunArrowName `DAppT` k1 `DAppT` k2
    go _        ty@DLitT{} = pure ty

    go args     hd = fail $ "Illegal Haskell construct encountered:\n" ++
                            "headed by: " ++ show hd ++ "\n" ++
                            "applied to: " ++ show args

-- | Promote a DTypeArg to the kind level. This is suffixed with "_NC" because
-- we do not invoke checkVanillaDType here.
-- See [Vanilla-type validity checking during promotion].
promoteTypeArg_NC :: OptionsMonad m => DTypeArg -> m DTypeArg
promoteTypeArg_NC (DTANormal t) = DTANormal <$> promoteType_NC t
promoteTypeArg_NC ta@(DTyArg _) = pure ta -- Kinds are already promoted

-- | Promote a DType to the kind level, splitting it into its type variable
-- binders, argument types, and result type in the process.
promoteUnraveled :: OptionsMonad m
                 => DType -> m ([DTyVarBndrSpec], [DKind], DKind)
promoteUnraveled ty = do
  (tvbs, _, arg_tys, res_ty) <- unravelVanillaDType ty
  arg_kis <- mapM promoteType_NC arg_tys
  res_ki  <- promoteType_NC res_ty
  return (tvbs, arg_kis, res_ki)

{-
Note [Vanilla-type validity checking during promotion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only support promoting (and singling) vanilla types, where a vanilla
function type is a type that:

1. Only uses a @forall@ at the top level, if used at all. That is to say, it
   does not contain any nested or higher-rank @forall@s.

2. Only uses a context (e.g., @c => ...@) at the top level, if used at all,
   and only after the top-level @forall@ if one is present. That is to say,
   it does not contain any nested or higher-rank contexts.

3. Contains no visible dependent quantification.

The checkVanillaDType function checks if a type is vanilla. Note that it is
crucial to call checkVanillaDType on the /entire/ type. For instance, it would
be incorrect to call unravelVanillaDType and then check each argument type
individually, since that loses information about which @forall@s/constraints
are higher-rank.

We make an effort to avoiding calling checkVanillaDType on the same type twice,
since checkVanillaDType must traverse the entire type. (It would not be
incorrect to do so, just wasteful.) For this certain, certain functions are
suffixed with "_NC" (short for "no checking") to indicate that they do not
invoke checkVanillaDType. These functions are used on types that have already
been validity-checked.
-}
