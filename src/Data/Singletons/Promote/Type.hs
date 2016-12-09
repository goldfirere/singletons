{- Data/Singletons/Type.hs

(c) Richard Eisenberg 2013
eir@cis.upenn.edu

This file implements promotion of types into kinds.
-}

module Data.Singletons.Promote.Type ( promoteType, promoteUnraveled ) where

import Language.Haskell.TH.Desugar
import Data.Singletons.Names
import Data.Singletons.Util
import Language.Haskell.TH

-- the only monadic thing we do here is fail. This allows the function
-- to be used from the Singletons module
promoteType :: Monad m => DType -> m DKind
promoteType = go []
  where
    go :: Monad m => [DKind] -> DType -> m DKind
    -- We don't need to worry about constraints: they are used to express
    -- static guarantees at runtime. But, because we don't need to do
    -- anything special to keep static guarantees at compile time, we don't
    -- need to promote them.
    go []       (DForallT _tvbs _cxt ty) = go [] ty
    go []       (DAppT (DAppT DArrowT (DForallT (_:_) _ _)) _) =
      fail "Cannot promote types of rank above 1."
    go args     (DAppT t1 t2) = do
      k2 <- go [] t2
      go (k2 : args) t1
    go args     (DSigT ty ki) = do
      ty' <- go [] ty
      -- No need to promote 'ki' - it is already a kind.
      return $ foldType (DSigT ty' ki) args
    go args     (DVarT name) = return $ foldType (DVarT name) args
    go []       (DConT name)
      | name == typeRepName               = return DStarT
      | name == stringName                = return $ DConT symbolName
      | nameBase name == nameBase repName = return DStarT
    go args     (DConT name)
      | Just n <- unboxedTupleNameDegree_maybe name
      = return $ foldType (DConT (tupleTypeName n)) args
      | otherwise
      = return $ foldType (DConT name) args
    go [k1, k2] DArrowT = return $ addStar (DConT tyFunName `DAppT` k1 `DAppT` k2)
    go _ (DLitT _) = fail "Cannot promote a type-level literal"

    go args     hd = fail $ "Illegal Haskell construct encountered:\n" ++
                            "headed by: " ++ show hd ++ "\n" ++
                            "applied to: " ++ show args

promoteUnraveled :: Monad m => DType -> m ([DKind], DKind)
promoteUnraveled ty = do
  arg_kis <- mapM promoteType arg_tys
  res_ki  <- promoteType res_ty
  return (arg_kis, res_ki)
  where
    (_, _, arg_tys, res_ty) = unravel ty
