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

-- @inferConstraints cls inst_ty cons@ infers the instance context for a
-- derived type class instance of @cls@ for @inst_ty@, using the constructors
-- @cons@. For instance, if @cls@ is 'Ord' and @inst_ty@ is @Either a b@, then
-- that means we are attempting to derive the instance:
--
-- @
-- instance ??? => Ord (Either a b)
-- @
--
-- The role of 'inferConstraints' is to determine what @???@ should be in that
-- derived instance. To accomplish this, the list of @cons@ (in this example,
-- @cons@ would be @[Left a, Right b]@) is used as follows:
--
-- 1. For each @con@ in @cons@, find the types of each of its fields
--    (call these @field_tys@), perhaps after renaming the type variables of
--    @field_tys@.
-- 2. For each @field_ty@ in @field_tys@, apply @cls@ to @field_ty@ to obtain
--    a constraint.
-- 3. The final instance context is the set of all such constraints obtained
--    in step 2.
--
-- To complete the running example, this algorithm would produce the instance
-- context @(Ord a, Ord b)@, since @Left a@ has one field of type @a@, and
-- @Right b@ has one field of type @b@.
--
-- This algorithm is a crude approximation of what GHC actually does when
-- deriving instances. It is crude in the sense that one can end up with
-- redundant constraints. For instance, if the data type for which an 'Ord'
-- instance is being derived is @data Foo = MkFoo Bool Foo@, then the
-- inferred constraints would be @(Ord Bool, Ord Foo)@. Technically, neither
-- constraint is necessary, but it is not simple in general to eliminate
-- redundant constraints like these, so we do not attept to do so. (This is
-- one reason why @singletons@ requires the use of the @UndecidableInstances@
-- GHC extension.)
--
-- Observant readers will notice that the phrase \"perhaps afer renaming the
-- type variables\" was casually dropped in step 1 of the above algorithm.
-- For more information on what this means, refer to the documentation for
-- infer_ct below.
inferConstraints :: forall q. DsMonad q => DPred -> DType -> [DCon] -> q DCxt
inferConstraints pr inst_ty = fmap (nubBy geq) . concatMapM infer_ct
  where
    -- A thorny situation arises when attempting to infer an instance context
    -- for a GADT. Consider the following example:
    --
    --   newtype Bar a where
    --     MkBar :: b -> Bar b
    --   deriving Show
    --
    -- If we blindly apply 'Show' to the field type of @MkBar@, we will end up
    -- with a derived instance of:
    --
    --   instance Show b => Show (Bar a)
    --
    -- This is completely wrong, since the type variable @b@ is never used in
    -- the instance head! This reveals that we need a slightly more nuanced
    -- strategy for gathering constraints for GADT constructors. To account
    -- for this, when gathering @field_tys@ (from step 1 in the above algorithm)
    -- we perform the following extra steps:
    --
    -- 1(a). Take the return type of @con@ and unify it with @inst_ty@ (e.g.,
    --       unify @Bar b@ with @Bar a@). Doing so will produce a substitution
    --       that maps the universally quantified type variables in the GADT
    --       (i.e., @b@) to the corresponding type variables in the data type
    --       constructor (i.e., @a@).
    -- 1(b). Use the resulting substitution to rename the universally
    --       quantified type variables of @con@ as necessary.
    --
    -- After this renaming, the algorithm will produce an instance context of
    -- @Show a@ (since @b@ was renamed to @a@), as expected.
    infer_ct :: DCon -> q DCxt
    infer_ct (DCon _ _ _ fields mb_res_ty) = do
      let field_tys = tysOfConFields fields
      field_tys' <- case mb_res_ty of
                      Nothing -> pure field_tys
                      Just res_ty -> do subst <- unify res_ty inst_ty
                                        pure $ map (substType subst) field_tys
      pure $ map (pr `DAppPr`) field_tys'

-- For @inferConstraintsDef mb_cxt@, if @mb_cxt@ is 'Just' a context, then it will
-- simply return that context. Otherwise, if @mb_cxt@ is 'Nothing', then
-- 'inferConstraintsDef' will infer an instance context (using 'inferConstraints').
inferConstraintsDef :: DsMonad q => Maybe DCxt -> DPred -> DType -> [DCon] -> q DCxt
inferConstraintsDef mb_ctxt pr inst_ty cons =
  maybe (inferConstraints pr inst_ty cons) pure mb_ctxt

-- A thin wrapper around @unify'@ which expand through type synonyms in the
-- argument types beforehand, and if unification fails, produces a sensible
-- error message.
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

-- An extremely simple unification algorithm. If unification succeeds, it will
-- produce a 'Right' substitution which maps type variable names to types.
-- Otherwise, it will fail with a 'Left' value containing the two subtypes
-- which failed to unify.
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
