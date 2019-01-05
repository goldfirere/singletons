{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Infer
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Infers constraints for a `deriving` class
--
----------------------------------------------------------------------------

module Data.Singletons.Deriving.Infer ( inferConstraints, inferConstraintsDef ) where

import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax
import Data.Singletons.Deriving.Util
import Data.Singletons.Util
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Generics.Twins

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
    -- 1(a). Take the return type of @con@ and match it with @inst_ty@ (e.g.,
    --       match @Bar b@ with @Bar a@). Doing so will produce a substitution
    --       that maps the universally quantified type variables in the GADT
    --       (i.e., @b@) to the corresponding type variables in the data type
    --       constructor (i.e., @a@).
    -- 1(b). Use the resulting substitution to rename the universally
    --       quantified type variables of @con@ as necessary.
    --
    -- After this renaming, the algorithm will produce an instance context of
    -- @Show a@ (since @b@ was renamed to @a@), as expected.
    infer_ct :: DCon -> q DCxt
    infer_ct (DCon _ _ _ fields res_ty) = do
      let field_tys = tysOfConFields fields
          -- We need to match the constructor's result type with the type given
          -- in the generated instance. But if we have:
          --
          --   data Foo a where
          --     MkFoo :: a -> Foo a
          --     deriving Functor
          --
          -- Then the generated instance will be:
          --
          --   instance Functor Foo where ...
          --
          -- Which means that if we're not careful, we might try to match the
          -- types (Foo a) and (Foo), which will fail.
          --
          -- To avoid this, we employ a grimy hack where we pad the instance
          -- type with an extra (dummy) type variable. It doesn't matter what
          -- we name it, since none of the inferred constraints will mention
          -- it anyway.
          eta_expanded_inst_ty
            | is_functor_like = inst_ty `DAppT` DVarT (mkName "dummy")
            | otherwise       = inst_ty
      res_ty'  <- expandType res_ty
      inst_ty' <- expandType eta_expanded_inst_ty
      field_tys' <- case matchTy YesIgnore res_ty' inst_ty' of
                      Nothing -> fail $ showString "Unable to match type "
                                      . showsPrec 11 res_ty'
                                      . showString " with "
                                      . showsPrec 11 inst_ty'
                                      $ ""
                      Just subst -> traverse (substTy subst) field_tys
      if is_functor_like
         then mk_functor_like_constraints field_tys' res_ty'
         else pure $ map (pr `DAppT`) field_tys'

    -- If we derive a Functor-like class, e.g.,
    --
    --   data Foo f g h a = MkFoo (f a) (g (h a)) deriving Functor
    --
    -- Then we infer constraints by sticking Functor on the subtypes of kind
    -- (Type -> Type). In the example above, that would give us
    -- (Functor f, Functor g, Functor h).
    mk_functor_like_constraints :: [DType] -> DType -> q DCxt
    mk_functor_like_constraints fields res_ty = do
      -- This function is partial. But that's OK, because
      -- functorLikeValidityChecks ensures that this is total by the time
      -- we invoke this.
      let _ :| res_ty_args     = unfoldType res_ty
          (_, last_res_ty_arg) = snocView res_ty_args
          Just last_tv         = getDVarTName_maybe last_res_ty_arg
      deep_subtypes <- concatMapM (deepSubtypesContaining last_tv) fields
      pure $ map (pr `DAppT`) deep_subtypes

    is_functor_like :: Bool
    is_functor_like
      | DConT pr_class_name :| _ <- unfoldType pr
      = isFunctorLikeClassName pr_class_name
      | otherwise
      = False

-- For @inferConstraintsDef mb_cxt@, if @mb_cxt@ is 'Just' a context, then it will
-- simply return that context. Otherwise, if @mb_cxt@ is 'Nothing', then
-- 'inferConstraintsDef' will infer an instance context (using 'inferConstraints').
inferConstraintsDef :: DsMonad q => Maybe DCxt -> DPred -> DType -> [DCon] -> q DCxt
inferConstraintsDef mb_ctxt pr inst_ty cons =
  maybe (inferConstraints pr inst_ty cons) pure mb_ctxt
