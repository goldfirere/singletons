-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Partition
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Partitions a list of declarations into its bits
--
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Singletons.Partition where

import Prelude hiding ( exp )
import Data.Singletons.Syntax
import Data.Singletons.Deriving.Ord
import Data.Singletons.Deriving.Bounded
import Data.Singletons.Deriving.Enum
import Data.Singletons.Deriving.Show
import Data.Singletons.Names
import Language.Haskell.TH.Syntax hiding (showName)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Desugar
import Data.Singletons.Util

import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup (Semigroup(..))

data PartitionedDecs =
  PDecs { pd_let_decs :: [DLetDec]
        , pd_class_decs :: [UClassDecl]
        , pd_instance_decs :: [UInstDecl]
        , pd_data_decs :: [DataDecl]
        , pd_ty_syn_decs :: [TySynDecl]
        , pd_open_type_family_decs :: [OpenTypeFamilyDecl]
        , pd_closed_type_family_decs :: [ClosedTypeFamilyDecl]
        , pd_derived_eq_decs :: [DerivedEqDecl]
        , pd_derived_show_decs :: [DerivedShowDecl]
        }

instance Semigroup PartitionedDecs where
  PDecs a1 b1 c1 d1 e1 f1 g1 h1 i1 <> PDecs a2 b2 c2 d2 e2 f2 g2 h2 i2 =
    PDecs (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)
          (g1 <> g2) (h1 <> h2) (i1 <> i2)

instance Monoid PartitionedDecs where
  mempty = PDecs [] [] [] [] [] [] [] [] []
  mappend = (<>)

-- | Split up a @[DDec]@ into its pieces, extracting 'Ord' instances
-- from deriving clauses
partitionDecs :: DsMonad m => [DDec] -> m PartitionedDecs
partitionDecs = concatMapM partitionDec

partitionDec :: DsMonad m => DDec -> m PartitionedDecs
partitionDec (DLetDec (DPragmaD {})) = return mempty
partitionDec (DLetDec letdec) = return $ mempty { pd_let_decs = [letdec] }

partitionDec (DDataD nd _cxt name tvbs mk cons derivings) = do
  extra_tvbs <- mkExtraDKindBinders $ fromMaybe (DConT typeKindName) mk
  let all_tvbs = tvbs ++ extra_tvbs
      data_dec = mempty { pd_data_decs = [DataDecl nd name all_tvbs cons []] }
      ty = foldTypeTvbs (DConT name) all_tvbs
  non_vanilla <- isNonVanillaDataType ty cons
  derived_decs
    <- mapM (\(strat, deriv_pred) -> partitionDeriving strat deriv_pred Nothing ty
                                                       non_vanilla cons)
      $ concatMap flatten_clause derivings
  return $ mconcat $ data_dec : derived_decs
  where
    flatten_clause :: DDerivClause -> [(Maybe DerivStrategy, DType)]
    flatten_clause (DDerivClause strat preds) =
      map (\p -> (strat, predToType p)) preds

partitionDec (DClassD cxt name tvbs fds decs) = do
  (lde, otfs) <- concatMapM partitionClassDec decs
  return $ mempty { pd_class_decs = [ClassDecl { cd_cxt       = cxt
                                               , cd_name      = name
                                               , cd_tvbs      = tvbs
                                               , cd_fds       = fds
                                               , cd_lde       = lde }]
                  , pd_open_type_family_decs = otfs }
partitionDec (DInstanceD _ cxt ty decs) = do
  defns <- liftM catMaybes $ mapM partitionInstanceDec decs
  (name, tys) <- split_app_tys [] ty
  return $ mempty { pd_instance_decs = [InstDecl { id_cxt       = cxt
                                                 , id_name      = name
                                                 , id_arg_tys   = tys
                                                 , id_meths     = defns }] }
  where
    split_app_tys acc (DAppT t1 t2) = split_app_tys (t2:acc) t1
    split_app_tys acc (DConT name)  = return (name, acc)
    split_app_tys acc (DSigT t _)   = split_app_tys acc t
    split_app_tys _ _ = fail $ "Illegal instance head: " ++ show ty
partitionDec (DRoleAnnotD {}) = return mempty  -- ignore these
partitionDec (DTySynD name tvbs _type) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure $ mempty { pd_ty_syn_decs = [TySynDecl name tvbs] }
partitionDec (DClosedTypeFamilyD tf_head _) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure $ mempty { pd_closed_type_family_decs = [TypeFamilyDecl tf_head] }
partitionDec (DOpenTypeFamilyD tf_head) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure $ mempty { pd_open_type_family_decs = [TypeFamilyDecl tf_head] }
partitionDec (DTySynInstD {}) = pure mempty
  -- There's no need to track type family instances, since
  -- we already record the type family itself separately.
partitionDec (DStandaloneDerivD mb_strat ctxt ty) =
  case unfoldType ty of
    cls_pred_ty :| cls_tys
      | not (null cls_tys) -- We can't handle zero-parameter type classes
      , let cls_arg_tys  = init cls_tys
            data_ty      = last cls_tys
            data_ty_head = case unfoldType data_ty of ty_head :| _ -> ty_head
      , DConT data_tycon <- data_ty_head -- We can't handle deriving an instance for something
                                         -- other than a type constructor application
      -> do let cls_pred = foldType cls_pred_ty cls_arg_tys
            dinfo <- dsReify data_tycon
            case dinfo of
              Just (DTyConI (DDataD _ _ dn dtvbs dk dcons _) _) -> do
                orig_data_ty <- buildDataDType dn dtvbs dk
                non_vanilla  <- isNonVanillaDataType orig_data_ty dcons
                partitionDeriving mb_strat cls_pred (Just ctxt) data_ty non_vanilla dcons
              Just _ ->
                fail $ "Standalone derived instance for something other than a datatype: "
                       ++ show data_ty
              _ -> fail $ "Cannot find " ++ show data_ty
    _ -> return mempty
partitionDec dec =
  fail $ "Declaration cannot be promoted: " ++ pprint (decToTH dec)

partitionClassDec :: Monad m => DDec -> m (ULetDecEnv, [OpenTypeFamilyDecl])
partitionClassDec (DLetDec (DSigD name ty)) =
  pure (typeBinding name ty, mempty)
partitionClassDec (DLetDec (DValD (DVarPa name) exp)) =
  pure (valueBinding name (UValue exp), mempty)
partitionClassDec (DLetDec (DFunD name clauses)) =
  pure (valueBinding name (UFunction clauses), mempty)
partitionClassDec (DLetDec (DInfixD fixity name)) =
  pure (infixDecl fixity name, mempty)
partitionClassDec (DLetDec (DPragmaD {})) =
  pure (mempty, mempty)
partitionClassDec (DOpenTypeFamilyD tf_head) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure (mempty, [TypeFamilyDecl tf_head])
partitionClassDec (DTySynInstD {}) =
  -- There's no need to track associated type family default equations, since
  -- we already record the type family itself separately.
  pure (mempty, mempty)
partitionClassDec _ =
  fail "Only method declarations can be promoted within a class."

partitionInstanceDec :: Monad m => DDec -> m (Maybe (Name, ULetDecRHS))
partitionInstanceDec (DLetDec (DValD (DVarPa name) exp)) =
  return $ Just (name, UValue exp)
partitionInstanceDec (DLetDec (DFunD name clauses)) =
  return $ Just (name, UFunction clauses)
partitionInstanceDec (DLetDec (DPragmaD {})) = return Nothing
partitionInstanceDec (DTySynInstD {}) = pure Nothing
  -- There's no need to track associated type family instances, since
  -- we already record the type family itself separately.
partitionInstanceDec _ =
  fail "Only method bodies can be promoted within an instance."

partitionDeriving :: forall m. DsMonad m
                  => Maybe DerivStrategy -> DType -> Maybe DCxt -> DType
                  -> Bool -- Is this a non-vanilla data type? (See the
                          -- documentation for 'isNonVanillaDataType'.)
                  -> [DCon]
                  -> m PartitionedDecs
partitionDeriving mb_strat deriv_pred mb_ctxt ty non_vanilla cons =
  case unfoldType deriv_pred of
    DConT deriv_name :| arg_tys
         -- Here, we are more conservative than GHC: DeriveAnyClass only kicks
         -- in if the user explicitly chooses to do so with the anyclass
         -- deriving strategy
       | Just AnyclassStrategy <- mb_strat
      -> return $ mk_derived_inst
           InstDecl { id_cxt = fromMaybe [] mb_ctxt
                      -- For now at least, there's no point in attempting to
                      -- infer an instance context for DeriveAnyClass, since
                      -- the other language feature that requires it,
                      -- DefaultSignatures, can't be singled. Thus, inferring an
                      -- empty context will Just Work for all currently supported
                      -- default implementations.
                      --
                      -- (Of course, if a user specifies a context with
                      -- StandaloneDeriving, use that.)

                    , id_name      = deriv_name
                    , id_arg_tys   = arg_tys ++ [ty]
                    , id_meths     = [] }

       | Just NewtypeStrategy <- mb_strat
      -> do qReportWarning "GeneralizedNewtypeDeriving is ignored by `singletons`."
            return mempty

    -- Stock classes. These are derived only if `singletons` supports them
    -- (and, optionally, if an explicit stock deriving strategy is used)
    DConT deriv_name :| [] -- For now, all stock derivable class supported in
                           -- singletons take just one argument (the data
                           -- type itself)
       | stock_or_default
       , deriv_name == ordName
      -> mk_derived_inst <$> mk_instance' mkOrdInstance

       | stock_or_default
       , deriv_name == boundedName
      -> mk_derived_inst <$> mk_instance' mkBoundedInstance

       | stock_or_default
       , deriv_name == enumName
      -> mk_derived_inst <$> mk_instance mkEnumInstance

         -- See Note [DerivedDecl] in Data.Singletons.Syntax
       | stock_or_default
       , deriv_name == eqName
      -> return $ mk_derived_eq_inst $ mk_derived_decl mb_ctxt ty cons

         -- See Note [DerivedDecl] in Data.Singletons.Syntax
       | stock_or_default
       , deriv_name == showName
      -> do -- This will become PShow/SShow instances...
            inst_for_promotion <- mk_instance' (mkShowInstance ForPromotion)
            -- ...and this will become ShowSing/Show instances.
            let inst_for_ShowSing = mk_derived_decl mb_ctxt ty cons
            pure $ mempty { pd_instance_decs     = [inst_for_promotion]
                          , pd_derived_show_decs = [inst_for_ShowSing] }

         -- If we can't find a stock class, but the user bothered to use an
         -- explicit stock keyword, we can at least warn them about it.
       | Just StockStrategy <- mb_strat
      -> do qReportWarning $ "`singletons` doesn't recognize the stock class "
                             ++ nameBase deriv_name
            return mempty

    _ -> return mempty -- singletons doesn't support deriving this instance
  where
      mk_instance maker = maker non_vanilla mb_ctxt ty cons
      -- A variant of mk_instance that doesn't care whether the data type is
      -- a vanilla data type.
      mk_instance' maker = mk_instance (const maker)

      mk_derived_inst    dec = mempty { pd_instance_decs   = [dec] }
      mk_derived_eq_inst dec = mempty { pd_derived_eq_decs = [dec] }
      mk_derived_decl mb_ctxt' ty' cons' = DerivedDecl { ded_mb_cxt = mb_ctxt'
                                                       , ded_type   = ty'
                                                       , ded_cons   = cons' }
      stock_or_default = isStockOrDefault mb_strat

-- Is this being used with an explicit stock strategy, or no strategy at all?
isStockOrDefault :: Maybe DerivStrategy -> Bool
isStockOrDefault Nothing              = True
isStockOrDefault (Just StockStrategy) = True
isStockOrDefault (Just _)             = False

buildDataDType :: DsMonad q => Name -> [DTyVarBndr] -> Maybe DKind -> q DType
buildDataDType n tvbs mk = do
  extra_tvbs <- mkExtraDKindBinders $ fromMaybe (DConT typeKindName) mk
  let all_tvbs = tvbs ++ extra_tvbs
  pure $ foldTypeTvbs (DConT n) all_tvbs

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

{-
Note [Partitioning, type synonyms, and type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The process of singling does not produce any new declarations corresponding to
type synonyms or type families, so they are "ignored" in a sense. Nevertheless,
we explicitly track them during partitioning, since we want to create
defunctionalization symbols for them.

Also note that:

1. Other uses of type synonyms in singled code will be expanded away.
2. Other uses of type families in singled code are unlikely to work at present
   due to Trac #12564.
-}
