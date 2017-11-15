{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Infer
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Infers constraints for a `deriving` class
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Infer ( inferConstraints, inferConstraintsDef ) where

import Language.Haskell.TH (Name)
import Language.Haskell.TH.Desugar
import Data.Singletons.Util
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Generics.Twins
import qualified Data.Set as Set

inferConstraints :: forall q. DsMonad q => DPred -> DType -> [DCon] -> q DCxt
inferConstraints pr inst_ty = fmap (nubBy geq) . concatMapM infer_ct
  where
    infer_ct :: DCon -> q DCxt
    infer_ct (DCon _ _ _ fields mb_res_ty) = do
      let field_tys = tysOfConFields fields
      field_tys' <- case mb_res_ty of
                      Nothing -> pure field_tys
                      Just res_ty -> do subst <- unify res_ty inst_ty
                                        pure $ map (substType subst) field_tys
      pure $ map (pr `DAppPr`) field_tys'

inferConstraintsDef :: DsMonad q => Maybe DCxt -> DPred -> DType -> [DCon] -> q DCxt
inferConstraintsDef mb_ctxt pr inst_ty cons =
  maybe (inferConstraints pr inst_ty cons) pure mb_ctxt

unify :: DsMonad q => DType -> DType -> q (Map Name DType)
unify t1 t2 = do
  t1' <- expandType t1
  t2' <- expandType t2
  case unify' t1' t2' of
    Right m -> pure m
    Left (x, y) ->
      fail $ showString "Unable to unify types "
           . showsPrec 11 x
           . showString " and "
           . showsPrec 11 y
           $ ""

unify' :: DType -> DType -> Either (DType, DType)
                                   (Map Name DType)
unify' (DVarT m) (DVarT n)
  | m == n = pure Map.empty
unify' (DVarT m) t
  | m `Set.member` fvDType t = Left (DVarT m, t)
  | otherwise                = pure (Map.singleton m t)
unify' t (DVarT n)
  | n `Set.member` fvDType t = Left (DVarT n, t)
  | otherwise                = pure (Map.singleton n t)
-- For now, we don't attempt to unify kinds
unify' (DSigT t _) u = unify' t u
unify' t (DSigT u _) = unify' t u
unify' (DConT m) (DConT n)
  | m == n = pure Map.empty
unify' (DAppT f1 x1) (DAppT f2 x2) = do
  sub1 <- unify' f1 f2
  sub2 <- unify' (substType sub1 x1) (substType sub1 x2)
  pure (combineSubstitutions sub1 sub2)
unify' DArrowT DArrowT = pure Map.empty
unify' (DLitT m) (DLitT n)
  | m == n = pure Map.empty
unify' DStarT DStarT = pure Map.empty
-- Currently not handled: DForallT and DWildCardT
unify' t u = Left (t, u)

combineSubstitutions :: Map Name DType -> Map Name DType -> Map Name DType
combineSubstitutions x y = Map.union (fmap (substType y) x) y
