{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Deriving.Util
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Utilities used by the `deriving` machinery in singletons.
--
----------------------------------------------------------------------------
module Data.Singletons.Deriving.Util where

import Control.Monad
import qualified Data.List as List
import Data.Singletons.Names
import Data.Singletons.Syntax
import Data.Singletons.Util
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OSet as OSet
import Language.Haskell.TH.Syntax

-- A generic type signature for describing how to produce a derived instance.
type DerivDesc q
   = Maybe DCxt  -- (Just ctx) if ctx was provided via StandaloneDeriving.
                 -- Nothing if using a deriving clause.
  -> DType       -- The data type argument to the class.
  -> DataDecl    -- The original data type information.
  -> q UInstDecl -- The derived instance.

-- | Is this data type a non-vanilla data type? Here, \"non-vanilla\" refers to
-- any data type that cannot be expressed using Haskell98 syntax. For instance,
-- this GADT:
--
-- @
-- data Foo :: Type -> Type where
--   MkFoo :: forall a. a -> Foo a
-- @
--
-- Is equivalent to this Haskell98 data type:
--
-- @
-- data Foo a = MkFoo a
-- @
--
-- However, the following GADT is non-vanilla:
--
-- @
-- data Bar :: Type -> Type where
--   MkBar :: Int -> Bar Int
-- @
--
-- Since there is no equivalent Haskell98 data type. The closest you could get
-- is this:
--
-- @
-- data Bar a = (a ~ Int) => MkBar Int
-- @
--
-- Which requires language extensions to write.
--
-- A data type is a non-vanilla if one of the following conditions are met:
--
-- 1. A constructor has any existentially quantified type variables.
--
-- 2. A constructor has a context.
--
-- We care about this because some derivable stock classes, such as 'Enum',
-- forbid derived instances for non-vanilla data types.
isNonVanillaDataType :: forall q. DsMonad q => DType -> [DCon] -> q Bool
isNonVanillaDataType data_ty = anyM $ \con@(DCon _ ctxt _ _ _) -> do
    ex_tvbs <- conExistentialTvbs data_ty con
    return $ not $ null ex_tvbs && null ctxt
  where
    anyM :: (a -> q Bool) -> [a] -> q Bool
    anyM _ [] = return False
    anyM p (x:xs) = do
      b <- p x
      if b then return True else anyM p xs

-----
-- Utilities for deriving Functor-like classes.
-- Much of this was cargo-culted from the GHC source code.
-----

data FFoldType a      -- Describes how to fold over a DType in a functor like way
   = FT { ft_triv    :: a
          -- ^ Does not contain variable
        , ft_var     :: a
          -- ^ The variable itself
        , ft_ty_app  :: DType -> a -> a
          -- ^ Type app, variable only in last argument
        , ft_bad_app :: a
          -- ^ Type app, variable other than in last argument
        , ft_forall  :: [DTyVarBndr] -> a -> a
          -- ^ Forall type
        }

-- Note that in GHC, this function is pure. It must be monadic here since we:
--
-- (1) Expand type synonyms
-- (2) Detect type family applications
--
-- Which require reification in Template Haskell, but are pure in Core.
functorLikeTraverse :: forall q a.
                       DsMonad q
                    => Name        -- ^ Variable to look for
                    -> FFoldType a -- ^ How to fold
                    -> DType       -- ^ Type to process
                    -> q a
functorLikeTraverse var (FT { ft_triv = caseTrivial, ft_var = caseVar
                            , ft_ty_app = caseTyApp, ft_bad_app = caseWrongArg
                            , ft_forall = caseForAll })
                    ty
  = do ty' <- expandType ty
       (res, _) <- go ty'
       pure res
  where
    go :: DType
       -> q (a, Bool) -- (result of type a, does type contain var)
    go t@DAppT{} = do
      let (f, args) = unfoldDType t
          vis_args  = filterDTANormals args
      (_,   fc)  <- go f
      (xrs, xcs) <- mapAndUnzipM go vis_args
      let wrongArg  :: q (a, Bool)
          wrongArg = pure (caseWrongArg, True)
      if |  not (or xcs)
         -> trivial -- Variable does not occur
         -- At this point we know that xrs, xcs is not empty,
         -- and at least one xr is True
         |  fc || or (init xcs)
         -> wrongArg                    -- T (..var..)    ty
         |  otherwise                   -- T (..no var..) ty
         -> do itf <- isInTypeFamilyApp var f vis_args
               if itf -- We can't decompose type families, so
                      -- error if we encounter one here.
                  then wrongArg
                  else pure (caseTyApp (last vis_args) (last xrs), True)
    go (DAppKindT t k) = do
      (_, kc) <- go k
      if kc
         then pure (caseWrongArg, True)
         else go t
    go (DSigT t k) = do
      (_, kc) <- go k
      if kc
         then pure (caseWrongArg, True)
         else go t
    go (DVarT v)
      | v == var = pure (caseVar, True)
      | otherwise = trivial
    go (DForallT _ tvbs t) = do
      (tr, tc) <- go t
      if var `notElem` map extractTvbName tvbs && tc
         then pure (caseForAll tvbs tr, True)
         else trivial
    go (DConstrainedT _ t) =  go t
    go (DConT {}) = trivial
    go DArrowT    = trivial
    go (DLitT {}) = trivial
    go DWildCardT = trivial

    trivial :: q (a, Bool)
    trivial = pure (caseTrivial, False)

-- | Detect if a Name occurs as an argument to some type family. This makes an
-- effort to exclude /oversaturated/ arguments to type families. For instance,
-- if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
isInTypeFamilyApp :: forall q. DsMonad q => Name -> DType -> [DType] -> q Bool
isInTypeFamilyApp name tyFun tyArgs =
  case tyFun of
    DConT tcName -> go tcName
    _            -> pure False
  where
    go :: Name -> q Bool
    go tcName = do
      info <- dsReify tcName
      case info of
        Just (DTyConI dec _)
          |  DOpenTypeFamilyD (DTypeFamilyHead _ bndrs _ _) <- dec
          -> withinFirstArgs bndrs
          |  DClosedTypeFamilyD (DTypeFamilyHead _ bndrs _ _) _ <- dec
          -> withinFirstArgs bndrs
        _ -> pure False

    withinFirstArgs :: [a] -> q Bool
    withinFirstArgs bndrs =
      let firstArgs = take (length bndrs) tyArgs
          argFVs    = foldMap fvDType firstArgs
      in pure $ name `elem` argFVs

-- A crude approximation of cond_functorOK from GHC. This checks that:
--
-- (1) There's at least one type variable in the data type.
-- (2) It doesn't use the last type variable in the wrong place, e.g. data T a = MkT (X a a)
-- (3) It doesn't constrain the last type variable, e.g., data T a = Eq a => MkT a
functorLikeValidityChecks :: forall q. DsMonad q => Bool -> DataDecl -> q ()
functorLikeValidityChecks allowConstrainedLastTyVar (DataDecl n data_tvbs cons)
  | null data_tvbs -- (1)
  = fail $ "Data type " ++ nameBase n ++ " must have some type parameters"
  | otherwise
  = mapM_ check_con cons
  where
    check_con :: DCon -> q ()
    check_con con = do
      check_universal con
      checks <- foldDataConArgs (ft_check (extractName con)) con
      sequence_ checks

    -- (2)
    check_universal :: DCon -> q ()
    check_universal con@(DCon con_tvbs con_theta con_name _ res_ty)
      | allowConstrainedLastTyVar
      = pure ()
      | (_, res_ty_args) <- unfoldDType res_ty
      , (_, last_res_ty_arg) <- snocView $ filterDTANormals res_ty_args
      , Just last_tv <- getDVarTName_maybe last_res_ty_arg
      = do ex_tvbs <- conExistentialTvbs (foldTypeTvbs (DConT n) data_tvbs) con
           let univ_tvb_names = map extractTvbName con_tvbs List.\\ map extractTvbName ex_tvbs
           if last_tv `elem` univ_tvb_names
                && last_tv `OSet.notMember` foldMap fvDType con_theta
              then pure ()
              else fail $ badCon con_name existential
      | otherwise
      = fail $ badCon con_name existential

    -- (3)
    ft_check :: Name -> FFoldType (q ())
    ft_check con_name =
      FT { ft_triv    = pure ()
         , ft_var     = pure ()
         , ft_ty_app  = \_ x -> x
         , ft_bad_app = fail $ badCon con_name wrong_arg
         , ft_forall  = \_ x -> x
         }

    badCon :: Name -> String -> String
    badCon con_name msg = "Constructor " ++ nameBase con_name ++ " " ++ msg

    existential, wrong_arg :: String
    existential = "must be truly polymorphic in the last argument of the data type"
    wrong_arg   = "must use the type variable only as the last argument of a data type"

-- Return all syntactic subterms of a type that contain the given variable somewhere.
-- These are the things that should appear in Functor-like instance constraints.
deepSubtypesContaining :: DsMonad q => Name -> DType -> q [DType]
deepSubtypesContaining tv
  = functorLikeTraverse tv
        (FT { ft_triv    = []
            , ft_var     = []
            , ft_ty_app  = (:)
            , ft_bad_app = error "in other argument in deepSubtypesContaining"
            , ft_forall  = \tvbs xs -> filter (\x -> all (not_in_ty x) tvbs) xs })
  where
    not_in_ty :: DType -> DTyVarBndr -> Bool
    not_in_ty ty tvb = extractTvbName tvb `OSet.notMember` fvDType ty

-- Fold over the arguments of a data constructor in a Functor-like way.
foldDataConArgs :: forall q a. DsMonad q => FFoldType a -> DCon -> q [a]
foldDataConArgs ft (DCon _ _ _ fields res_ty) = do
  field_tys <- traverse expandType $ tysOfConFields fields
  traverse foldArg field_tys
  where
    foldArg :: DType -> q a
    foldArg
      | (_, res_ty_args) <- unfoldDType res_ty
      , (_, last_res_ty_arg) <- snocView $ filterDTANormals res_ty_args
      , Just last_tv <- getDVarTName_maybe last_res_ty_arg
      = functorLikeTraverse last_tv ft
      | otherwise
      = const (return (ft_triv ft))

-- If a type is a type variable (or a variable with a kind signature), return
-- 'Just' that. Otherwise, return 'Nothing'.
getDVarTName_maybe :: DType -> Maybe Name
getDVarTName_maybe (DSigT t _) = getDVarTName_maybe t
getDVarTName_maybe (DVarT n)   = Just n
getDVarTName_maybe _           = Nothing

-- Make a 'DLamE' using a fresh variable.
mkSimpleLam :: Quasi q => (DExp -> q DExp) -> q DExp
mkSimpleLam lam = do
  n <- newUniqueName "n"
  body <- lam (DVarE n)
  return $ DLamE [n] body

-- Make a 'DLamE' using two fresh variables.
mkSimpleLam2 :: Quasi q => (DExp -> DExp -> q DExp) -> q DExp
mkSimpleLam2 lam = do
  n1 <- newUniqueName "n1"
  n2 <- newUniqueName "n2"
  body <- lam (DVarE n1) (DVarE n2)
  return $ DLamE [n1, n2] body

-- "Con a1 a2 a3 -> fold [x1 a1, x2 a2, x3 a3]"
--
-- @mkSimpleConClause fold extra_pats con insides@ produces a match clause in
-- which the LHS pattern-matches on @extra_pats@, followed by a match on the
-- constructor @con@ and its arguments. The RHS folds (with @fold@) over @con@
-- and its arguments, applying an expression (from @insides@) to each of the
-- respective arguments of @con@.
mkSimpleConClause :: Quasi q
                  => (Name -> [DExp] -> DExp)
                  -> [DPat]
                  -> DCon
                  -> [DExp]
                  -> q DClause
mkSimpleConClause fold extra_pats (DCon _ _ con_name _ _) insides = do
  vars_needed <- replicateM (length insides) $ newUniqueName "a"
  let pat = DConP con_name (map DVarP vars_needed)
      rhs = fold con_name (zipWith (\i v -> i `DAppE` DVarE v) insides vars_needed)
  pure $ DClause (extra_pats ++ [pat]) rhs

-- 'True' if the derived class's last argument is of kind (Type -> Type),
-- and thus needs a different constraint inference approach.
--
-- Really, we should be determining this information by inspecting the kind
-- of the class being used. But that comes dangerously close to kind
-- inference territory, so for now we simply hardcode which stock derivable
-- classes are Functor-like.
isFunctorLikeClassName :: Name -> Bool
isFunctorLikeClassName class_name
  = class_name `elem` [functorName, foldableName, traversableName]
