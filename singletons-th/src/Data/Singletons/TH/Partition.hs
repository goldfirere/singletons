-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Partition
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Partitions a list of declarations into its bits
--
----------------------------------------------------------------------------

module Data.Singletons.TH.Partition where

import Prelude hiding ( exp )
import Data.Singletons.TH.Deriving.Bounded
import Data.Singletons.TH.Deriving.Enum
import Data.Singletons.TH.Deriving.Eq
import Data.Singletons.TH.Deriving.Foldable
import Data.Singletons.TH.Deriving.Functor
import Data.Singletons.TH.Deriving.Ord
import Data.Singletons.TH.Deriving.Show
import Data.Singletons.TH.Deriving.Traversable
import Data.Singletons.TH.Deriving.Util
import Data.Singletons.TH.Names
import Data.Singletons.TH.Options
import Data.Singletons.TH.Syntax
import Data.Singletons.TH.Util
import Language.Haskell.TH.Syntax hiding (showName)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Desugar
import qualified Language.Haskell.TH.Desugar.OMap.Strict as OMap
import Language.Haskell.TH.Desugar.OMap.Strict (OMap)

import Control.Monad
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

data PartitionedDecs =
  PDecs { pd_let_decs :: [DLetDec]
        , pd_class_decs :: [UClassDecl]
        , pd_instance_decs :: [UInstDecl]
        , pd_data_decs :: [DataDecl]
        , pd_ty_syn_decs :: [TySynDecl]
        , pd_open_type_family_decs :: [OpenTypeFamilyDecl]
        , pd_closed_type_family_decs :: [ClosedTypeFamilyDecl]
        , pd_derived_eq_decs :: [DerivedEqDecl]
        , pd_derived_ord_decs :: [DerivedOrdDecl]
        , pd_derived_show_decs :: [DerivedShowDecl]
        }

instance Semigroup PartitionedDecs where
  PDecs a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 <> PDecs a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 =
    PDecs (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)
          (f1 <> f2) (g1 <> g2) (h1 <> h2) (i1 <> i2) (j1 <> j2)

instance Monoid PartitionedDecs where
  mempty = PDecs mempty mempty mempty mempty mempty
                 mempty mempty mempty mempty mempty

-- | Split up a @[DDec]@ into its pieces, extracting 'Ord' instances
-- from deriving clauses
partitionDecs :: OptionsMonad m => [DDec] -> m PartitionedDecs
partitionDecs = concatMapM partitionDec

partitionDec :: OptionsMonad m => DDec -> m PartitionedDecs
partitionDec (DLetDec (DPragmaD {})) = return mempty
partitionDec (DLetDec letdec) = return $ mempty { pd_let_decs = [letdec] }

partitionDec (DDataD df _cxt name tvbs mk cons derivings) = do
  all_tvbs <- buildDataDTvbs tvbs mk
  let data_decl   = DataDecl df name all_tvbs cons
      derived_dec = mempty { pd_data_decs = [data_decl] }
  derived_decs
    <- mapM (\(strat, deriv_pred) ->
              let etad_tvbs
                    | (DConT pred_name, _) <- unfoldDType deriv_pred
                    , isFunctorLikeClassName pred_name
                      -- If deriving Functor, Foldable, or Traversable,
                      -- we need to use one less type variable than we normally do.
                    = take (length all_tvbs - 1) all_tvbs
                    | otherwise
                    = all_tvbs
                  ty = foldTypeTvbs (DConT name) etad_tvbs
              in partitionDeriving strat deriv_pred Nothing ty data_decl)
      $ concatMap flatten_clause derivings
  return $ mconcat $ derived_dec : derived_decs
  where
    flatten_clause :: DDerivClause -> [(Maybe DDerivStrategy, DPred)]
    flatten_clause (DDerivClause strat preds) =
      map (\p -> (strat, p)) preds

partitionDec (DClassD cxt name tvbs fds decs) = do
  (lde, otfs) <- concatMapM partitionClassDec decs
  return $ mempty { pd_class_decs = [ClassDecl { cd_cxt       = cxt
                                               , cd_name      = name
                                               , cd_tvbs      = tvbs
                                               , cd_fds       = fds
                                               , cd_lde       = lde
                                               , cd_atfs      = otfs}] }
partitionDec (DInstanceD _ _ cxt ty decs) = do
  (defns, sigs) <- liftM (bimap catMaybes mconcat) $
                   mapAndUnzipM partitionInstanceDec decs
  (name, tys) <- split_app_tys [] ty
  return $ mempty { pd_instance_decs = [InstDecl { id_cxt       = cxt
                                                 , id_name      = name
                                                 , id_arg_tys   = tys
                                                 , id_sigs      = sigs
                                                 , id_meths     = defns }] }
  where
    split_app_tys acc (DAppT t1 t2) = split_app_tys (t2:acc) t1
    split_app_tys acc (DConT name)  = return (name, acc)
    split_app_tys acc (DSigT t _)   = split_app_tys acc t
    split_app_tys _ _ = fail $ "Illegal instance head: " ++ show ty
partitionDec (DRoleAnnotD {}) = return mempty  -- ignore these
partitionDec (DTySynD name tvbs rhs) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure $ mempty { pd_ty_syn_decs = [TySynDecl name tvbs rhs] }
partitionDec (DClosedTypeFamilyD tf_head _) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure $ mempty { pd_closed_type_family_decs = [TypeFamilyDecl tf_head] }
partitionDec (DOpenTypeFamilyD tf_head) =
  -- See Note [Partitioning, type synonyms, and type families]
  pure $ mempty { pd_open_type_family_decs = [TypeFamilyDecl tf_head] }
partitionDec (DTySynInstD {}) = pure mempty
  -- There's no need to track type family instances, since
  -- we already record the type family itself separately.
partitionDec (DKiSigD {}) = pure mempty
  -- There's no need to track standalone kind signatures, since we use
  -- dsReifyType to look them up.
partitionDec (DStandaloneDerivD mb_strat _ ctxt ty) =
  case unfoldDType ty of
    (cls_pred_ty, cls_tys)
      | let cls_normal_tys = filterDTANormals cls_tys
      , not (null cls_normal_tys) -- We can't handle zero-parameter type classes
      , let cls_arg_tys  = init cls_normal_tys
            data_ty      = last cls_normal_tys
            data_ty_head = case unfoldDType data_ty of (ty_head, _) -> ty_head
      , DConT data_tycon <- data_ty_head -- We can't handle deriving an instance for something
                                         -- other than a type constructor application
      -> do let cls_pred = foldType cls_pred_ty cls_arg_tys
            dinfo <- dsReify data_tycon
            case dinfo of
              Just (DTyConI (DDataD df _ dn dtvbs dk dcons _) _) -> do
                all_tvbs <- buildDataDTvbs dtvbs dk
                let data_decl = DataDecl df dn all_tvbs dcons
                partitionDeriving mb_strat cls_pred (Just ctxt) data_ty data_decl
              Just _ ->
                fail $ "Standalone derived instance for something other than a datatype: "
                       ++ show data_ty
              _ -> fail $ "Cannot find " ++ show data_ty
    _ -> return mempty
partitionDec dec =
  fail $ "Declaration cannot be promoted: " ++ pprint (decToTH dec)

partitionClassDec :: MonadFail m => DDec -> m (ULetDecEnv, [OpenTypeFamilyDecl])
partitionClassDec (DLetDec (DSigD name ty)) =
  pure (typeBinding name ty, mempty)
partitionClassDec (DLetDec (DValD (DVarP name) exp)) =
  pure (valueBinding name (UValue exp), mempty)
partitionClassDec (DLetDec (DFunD name clauses)) =
  pure (valueBinding name (UFunction clauses), mempty)
partitionClassDec (DLetDec (DInfixD fixity ns name)) =
  pure (infixDecl fixity ns name, mempty)
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

partitionInstanceDec :: MonadFail m => DDec
                     -> m ( Maybe (Name, ULetDecRHS) -- right-hand sides of methods
                          , OMap Name DType          -- method type signatures
                          )
partitionInstanceDec (DLetDec (DValD (DVarP name) exp)) =
  pure (Just (name, UValue exp), mempty)
partitionInstanceDec (DLetDec (DFunD name clauses)) =
  pure (Just (name, UFunction clauses), mempty)
partitionInstanceDec (DLetDec (DSigD name ty)) =
  pure (Nothing, OMap.singleton name ty)
partitionInstanceDec (DLetDec (DPragmaD {})) =
  pure (Nothing, mempty)
partitionInstanceDec (DTySynInstD {}) =
  pure (Nothing, mempty)
  -- There's no need to track associated type family instances, since
  -- we already record the type family itself separately.
partitionInstanceDec _ =
  fail "Only method bodies can be promoted within an instance."

partitionDeriving
  :: forall m. OptionsMonad m
  => Maybe DDerivStrategy
                -- ^ The deriving strategy, if present.
  -> DPred      -- ^ The class being derived (e.g., 'Eq'), possibly applied to
                --   some number of arguments (e.g., @C Int Bool@).
  -> Maybe DCxt -- ^ @'Just' ctx@ if @ctx@ was provided via @StandaloneDeriving@.
                --   'Nothing' if using a @deriving@ clause.
  -> DType      -- ^ The data type argument to the class.
  -> DataDecl   -- ^ The original data type information (e.g., its constructors).
  -> m PartitionedDecs
partitionDeriving mb_strat deriv_pred mb_ctxt ty data_decl =
  case unfoldDType deriv_pred of
    (DConT deriv_name, arg_tys)
         -- Here, we are more conservative than GHC: DeriveAnyClass only kicks
         -- in if the user explicitly chooses to do so with the anyclass
         -- deriving strategy
       | Just DAnyclassStrategy <- mb_strat
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
                    , id_arg_tys   = filterDTANormals arg_tys ++ [ty]
                    , id_sigs      = mempty
                    , id_meths     = [] }

       | Just DNewtypeStrategy <- mb_strat
      -> do qReportWarning "GeneralizedNewtypeDeriving is ignored by `singletons-th`."
            return mempty

       | Just (DViaStrategy {}) <- mb_strat
      -> do qReportWarning "DerivingVia is ignored by `singletons-th`."
            return mempty

    -- Stock classes. These are derived only if `singletons-th` supports them
    -- (and, optionally, if an explicit stock deriving strategy is used)
    (DConT deriv_name, []) -- For now, all stock derivable class supported in
                           -- singletons-th take just one argument (the data
                           -- type itself)
       | stock_or_default
       , Just decs <- Map.lookup deriv_name stock_map
      -> decs

         -- If we can't find a stock class, but the user bothered to use an
         -- explicit stock keyword, we can at least warn them about it.
       | Just DStockStrategy <- mb_strat
      -> do qReportWarning $ "`singletons-th` doesn't recognize the stock class "
                             ++ nameBase deriv_name
            return mempty

    _ -> return mempty -- singletons-th doesn't support deriving this instance
  where
      mk_instance :: DerivDesc m -> m UInstDecl
      mk_instance maker = maker mb_ctxt ty data_decl

      mk_derived_inst    dec = mempty { pd_instance_decs   = [dec] }

      derived_decl :: DerivedDecl cls
      derived_decl = DerivedDecl { ded_mb_cxt     = mb_ctxt
                                 , ded_type       = ty
                                 , ded_type_tycon = ty_tycon
                                 , ded_decl       = data_decl }
        where
          ty_tycon :: Name
          ty_tycon = case unfoldDType ty of
                       (DConT tc, _) -> tc
                       (t,        _) -> error $ "Not a data type: " ++ show t
      stock_or_default = isStockOrDefault mb_strat

      -- A mapping from all stock derivable classes (that singletons-th supports)
      -- to to derived code that they produce.
      stock_map :: Map Name (m PartitionedDecs)
      stock_map = Map.fromList
        [ ( ordName,         mk_derived_inst <$> mk_instance mkOrdInstance )
        , ( boundedName,     mk_derived_inst <$> mk_instance mkBoundedInstance )
        , ( enumName,        mk_derived_inst <$> mk_instance mkEnumInstance )
        , ( functorName,     mk_derived_inst <$> mk_instance mkFunctorInstance )
        , ( foldableName,    mk_derived_inst <$> mk_instance mkFoldableInstance )
        , ( traversableName, mk_derived_inst <$> mk_instance mkTraversableInstance )

          -- See Note [DerivedDecl] in Data.Singletons.TH.Syntax
        , ( eqName,   do -- These will become PEq/SEq instances...
                         inst_for_promotion <- mk_instance mkEqInstance
                         -- ...and these will become SDecide/Eq/TestEquality/TestCoercion instances.
                         let inst_for_decide = derived_decl
                         return $ mempty { pd_instance_decs   = [inst_for_promotion]
                                         , pd_derived_eq_decs = [inst_for_decide] } )
        , ( ordName,  do -- These will become POrd/SOrd instances...
                         inst_for_promotion <- mk_instance mkOrdInstance
                         -- ...and this will become an Ord instance.
                         let inst_for_ord = derived_decl
                         pure $ mempty { pd_instance_decs    = [inst_for_promotion]
                                       , pd_derived_ord_decs = [inst_for_ord] } )
        , ( showName, do -- These will become PShow/SShow instances...
                         inst_for_promotion <- mk_instance mkShowInstance
                         -- ...and this will become a Show instance.
                         let inst_for_show = derived_decl
                         pure $ mempty { pd_instance_decs     = [inst_for_promotion]
                                       , pd_derived_show_decs = [inst_for_show] } )
        ]

-- Is this being used with an explicit stock strategy, or no strategy at all?
isStockOrDefault :: Maybe DDerivStrategy -> Bool
isStockOrDefault Nothing               = True
isStockOrDefault (Just DStockStrategy) = True
isStockOrDefault (Just _)              = False

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
3. We track open type families, closed type families, and associated type
   families separately, as each form of type family has different kind
   inference behavior. See defunTopLevelTypeDecls and
   defunAssociatedTypeFamilies in D.S.TH.Promote.Defun for how these differences
   manifest.
-}
